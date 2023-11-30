use crate::term::{
  check::type_check::{infer_arg_type, Type},
  Adt, Book, DefId, DefNames, Definition, MatchNum, Name, Pattern, Rule, Term,
};
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  pub fn simplify_matches(&mut self) -> Result<(), String> {
    let mut new_rules = BTreeMap::<DefId, Definition>::new();
    for (def_id, def) in &mut self.defs {
      let def_name = self.def_names.name(def_id).unwrap().clone();
      for rule in def.rules.iter_mut() {
        rule.body.simplify_matches(
          &def_name,
          &self.adts,
          &self.ctrs,
          &mut self.def_names,
          &mut new_rules,
          &mut 0,
        )?;
      }
    }
    self.defs.append(&mut new_rules);
    Ok(())
  }
}

impl Term {
  pub fn check_matches<'a>(
    pats: &[Pattern],
    adts: &'a BTreeMap<Name, Adt>,
    ctrs: &HashMap<Name, Name>,
  ) -> Result<&'a Adt, String> {
    let ty = infer_arg_type(pats.iter(), ctrs)?;

    let Type::Adt(nam) = ty else { unreachable!() };

    let Adt { ctrs } = &adts[&nam];

    let mut names = HashSet::new();
    let mut repeated = HashSet::new();
    let mut missing: HashSet<_> = ctrs.keys().collect();

    for rule in pats {
      let Pattern::Ctr(nam, _) = rule else { unreachable!() };

      if !names.insert(nam.clone()) {
        repeated.insert(nam.clone());
      }

      missing.remove(nam);
    }

    fn ctrs_plural_or_sing(n: usize) -> &'static str {
      if n > 1 { "constructors" } else { "a constructor" }
    }

    if !repeated.is_empty() {
      let constructor = ctrs_plural_or_sing(repeated.len());
      let repeated = repeated.into_iter().join(", ");
      return Err(format!("Repeated {constructor} in a match block: {repeated}"));
    }

    if !missing.is_empty() {
      let constructor = ctrs_plural_or_sing(missing.len());
      let missing = missing.into_iter().join(", ");
      return Err(format!("Missing {constructor} in a match block: {missing}"));
    }

    Ok(&adts[&nam])
  }

  pub fn simplify_matches(
    &mut self,
    def_name: &Name,
    adts: &BTreeMap<Name, Adt>,
    ctrs: &HashMap<Name, Name>,
    def_names: &mut DefNames,
    new_rules: &mut BTreeMap<DefId, Definition>,
    match_count: &mut usize,
  ) -> Result<(), String> {
    match self {
      Term::Match { arms, .. } => {
        if arms.is_empty() {
          return Err("Empty match block found".to_string());
        }

        for (_, term) in arms.iter_mut() {
          term.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count)?;
        }

        *match_count += 1;

        let Term::Match { scrutinee, arms } = std::mem::take(self) else { unreachable!() };
        let Term::Var { nam } = *scrutinee else { unreachable!() };

        if matches!(arms[0], (Pattern::Num(_), _)) {
          *self = match_native(nam, arms, def_name, def_names, new_rules, *match_count);
        } else {
          let rules: Vec<_> = arms
            .iter()
            .map(|(rule, _)| match rule {
              Pattern::Var(Some(nam)) => Pattern::Ctr(nam.clone(), Vec::new()),
              _ => unreachable!(),
            })
            .collect();

          let adt = Term::check_matches(&rules, adts, ctrs)?;
          *self = match_adt_app(nam, adt, &arms, def_name, def_names, new_rules, *match_count);
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count)?;
      }

      Term::App { fun: fst, arg: snd }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count)?;
        snd.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count)?;
      }

      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }

    Ok(())
  }
}

/// Split each arm of a native number match on its own rule and reconstructs the match term
fn match_native(
  scrutinee: Name,
  arms: Vec<(Pattern, Term)>,
  def_name: &Name,
  def_names: &mut DefNames,
  new_rules: &mut BTreeMap<DefId, Definition>,
  match_count: usize,
) -> Term {
  let mut new_arms = Vec::new();

  let mut free_vars = IndexSet::new();

  for (_, body) in &arms {
    free_vars.extend(body.free_vars().into_keys().filter(|Name(nam)| nam != &format!("{scrutinee}-1")));
  }

  for (rule, mut body) in arms {
    let (name, bind) = match &rule {
      Pattern::Num(MatchNum::Zero) => ("zero", None),
      Pattern::Num(MatchNum::Succ(Some(nam))) => ("succ", Some(nam.clone())),
      _ => unreachable!(), // Succ(None) should not happen here
    };

    if let Some(nam) = &bind {
      body = Term::Lam { nam: Some(nam.clone()), bod: Box::new(body) };
    }

    body =
      free_vars.iter().cloned().rev().fold(body, |acc, f| Term::Lam { nam: Some(f), bod: Box::new(acc) });

    let name = make_def_name(def_name, &Name(name.to_string()), match_count);
    let def_id = def_names.insert(name);
    let rules = vec![Rule { pats: Vec::new(), body }];
    let def = Definition { def_id, rules };
    new_rules.insert(def_id, def);

    let mut body = Term::Ref { def_id };

    if let Some(nam) = bind {
      body = Term::App { fun: Box::new(body), arg: Box::new(Term::Var { nam }) }
    }

    new_arms.push((rule, body));
  }

  free_vars
    .into_iter()
    .fold(Term::Match { scrutinee: Box::new(Term::Var { nam: scrutinee }), arms: new_arms }, |acc, nam| {
      Term::App { fun: Box::new(acc), arg: Box::new(Term::Var { nam }) }
    })
}

/// Split each arm of an adt match on its own rule,
/// returning a scott encoded term of the aplication of the scrutinee to each rule
fn match_adt_app(
  scrutinee: Name,
  Adt { ctrs }: &Adt,
  arms: &[(Pattern, Term)],
  def_name: &Name,
  def_names: &mut DefNames,
  new_rules: &mut BTreeMap<DefId, Definition>,
  match_count: usize,
) -> Term {
  let mut refs_to_app = Vec::new();
  let mut ordered_arms = Vec::new();
  let mut free_vars = IndexSet::new();

  for (ctr_name, args) in ctrs {
    for (rule, term) in arms {
      let Pattern::Var(Some(ctr)) = rule else { unreachable!() };

      if ctr == ctr_name {
        let mut term = term.clone();

        term.subst(
          &scrutinee,
          &Term::call(
            Term::Ref { def_id: def_names.def_id(ctr).unwrap() },
            args.iter().map(|arg| Term::Var { nam: binded(&scrutinee, arg) }),
          ),
        );

        let adt_binds: HashSet<_> = args.iter().map(|n| binded(&scrutinee, n)).collect();
        let free_vars_staging = term.free_vars().into_keys().filter(|k| !adt_binds.contains(k));

        free_vars.extend(free_vars_staging);
        ordered_arms.push((ctr, term));
      }
    }
  }

  for (ctr, term) in ordered_arms {
    let body = free_vars
      .iter()
      .cloned()
      .rev()
      .fold(term, |acc: Term, name| Term::Lam { nam: Some(name), bod: Box::new(acc) });

    let args = &ctrs[ctr];

    let body = args
      .iter()
      .rev()
      .fold(body, |acc, n| Term::Lam { nam: Some(binded(&scrutinee, n)), bod: Box::new(acc) });

    let rules = vec![Rule { pats: Vec::new(), body }];

    let def_name = make_def_name(def_name, ctr, match_count);
    let def_id = def_names.insert(def_name);
    let def = Definition { def_id, rules };
    new_rules.insert(def_id, def);
    refs_to_app.push(def_id);
  }

  free_vars.into_iter().fold(
    refs_to_app.into_iter().fold(Term::Var { nam: scrutinee }, |scrutinee, def_id| Term::App {
      fun: Box::new(scrutinee),
      arg: Box::new(Term::Ref { def_id }),
    }),
    |acc, nam| Term::App { fun: Box::new(acc), arg: Box::new(Term::Var { nam }) },
  )
}

fn binded(bind: &Name, acc: &Name) -> Name {
  Name(format!("{bind}.{acc}"))
}

fn make_def_name(def_name: &Name, ctr: &Name, i: usize) -> Name {
  Name(format!("{def_name}${ctr}${i}"))
}
