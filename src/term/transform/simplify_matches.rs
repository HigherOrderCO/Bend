use crate::term::{
  check::type_check::{infer_arg_type, Type},
  Adt, Book, DefId, DefNames, Definition, MatchNum, Name, Pattern, Rule, Tag, Term,
};
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  pub fn simplify_matches(&mut self) -> Result<(), String> {
    let mut new_defs = BTreeMap::<DefId, Definition>::new();
    for (def_id, def) in &mut self.defs {
      let def_name = self.def_names.name(def_id).unwrap().clone();
      for rule in def.rules.iter_mut() {
        rule
          .body
          .simplify_matches(&def_name, &self.adts, &self.ctrs, &mut self.def_names, &mut new_defs, &mut 0)
          .map_err(|err| err.to_string())?;
      }
    }
    self.defs.append(&mut new_defs);
    Ok(())
  }
}

#[derive(Debug)]
pub enum MatchError {
  Empty,
  Infer(String),
  RepeatedBind(Name),
  RepeatedCtrs(Vec<Name>),
  Missing(HashSet<Name>),
  LetPat(Box<MatchError>),
}

impl std::error::Error for MatchError {}

impl std::fmt::Display for MatchError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn ctrs_plural_or_sing(n: usize) -> &'static str {
      if n > 1 { "constructors" } else { "a constructor" }
    }

    match self {
      MatchError::Empty => write!(f, "Empty match block found"),
      MatchError::Infer(err) => write!(f, "{err}"),
      MatchError::RepeatedBind(bind) => write!(f, "Repeated var name in a match block: {}", bind),
      MatchError::RepeatedCtrs(names) => {
        let constructor = ctrs_plural_or_sing(names.len());
        let repeated = names.into_iter().join(", ");
        write!(f, "Repeated {constructor} in a match block: {repeated}")
      }
      MatchError::Missing(names) => {
        let constructor = ctrs_plural_or_sing(names.len());
        let missing = names.into_iter().join(", ");
        write!(f, "Missing {constructor} in a match block: {missing}")
      }
      MatchError::LetPat(err) => {
        let let_err = err.to_string().replace("match block", "let bind");
        write!(f, "{let_err}")?;

        if matches!(err.as_ref(), MatchError::Missing(_)) {
          write!(f, "\nConsider using a match block instead")?;
        }

        Ok(())
      }
    }
  }
}

impl Term {
  pub fn check_matches<'a>(
    pats: &[Pattern],
    adts: &'a BTreeMap<Name, Adt>,
    ctrs: &'a HashMap<Name, Name>,
  ) -> Result<&'a Adt, MatchError> {
    let ty = infer_arg_type(pats.iter(), ctrs).map_err(MatchError::Infer)?;

    let Type::Adt(nam) = ty else { unreachable!() };

    let Adt { ctrs } = &adts[&nam];

    let mut names = HashSet::new();
    let mut repeated = Vec::new();
    let mut missing: HashSet<_> = ctrs.keys().cloned().collect();
    let mut binds = HashSet::new();

    for rule in pats {
      let Pattern::Ctr(nam, args) = rule else { unreachable!() };

      for arg in args {
        for bind in arg.names() {
          match bind {
            Some(bind) if !binds.insert(bind) => {
              return Err(MatchError::RepeatedBind(bind.clone()));
            }
            _ => (),
          }
        }
      }

      if !names.insert(nam) {
        repeated.push(nam.clone());
      }

      missing.remove(nam);
    }

    if !repeated.is_empty() {
      return Err(MatchError::RepeatedCtrs(repeated));
    }

    if !missing.is_empty() {
      return Err(MatchError::Missing(missing));
    }

    Ok(&adts[&nam])
  }

  pub fn simplify_matches(
    &mut self,
    def_name: &Name,
    adts: &BTreeMap<Name, Adt>,
    ctrs: &HashMap<Name, Name>,
    def_names: &mut DefNames,
    new_defs: &mut BTreeMap<DefId, Definition>,
    match_count: &mut usize,
  ) -> Result<(), MatchError> {
    match self {
      Term::Match { arms, .. } => {
        if arms.is_empty() {
          return Err(MatchError::Empty);
        }

        for (_, term) in arms.iter_mut() {
          term.simplify_matches(def_name, adts, ctrs, def_names, new_defs, match_count)?;
        }

        *match_count += 1;

        let Term::Match { scrutinee, arms } = std::mem::take(self) else { unreachable!() };
        let Term::Var { nam } = *scrutinee else { unreachable!() };

        if matches!(arms[0], (Pattern::Num(_), _)) {
          *self = match_native(nam, arms, def_name, def_names, new_defs, *match_count);
        } else {
          let rules: Vec<_> = arms
            .iter()
            .map(|(rule, _)| match rule {
              Pattern::Var(Some(nam)) => Pattern::Ctr(nam.clone(), Vec::new()),
              Pattern::Ctr(..) => rule.clone(),
              _ => unreachable!(),
            })
            .collect();

          let adt = Term::check_matches(&rules, adts, ctrs)?;
          *self = match_adt_app(nam, adt, &arms, def_name, def_names, new_defs, *match_count);
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.simplify_matches(def_name, adts, ctrs, def_names, new_defs, match_count)?;
      }

      Term::Let { pat: Pattern::Ctr(_, _), .. } => {
        let Term::Let { pat, val, nxt } = std::mem::take(self) else { unreachable!() };
        let Term::Var { .. } = *val else { todo!() };

        *self = Term::Match { scrutinee: val, arms: vec![(pat, *nxt)] };

        self
          .simplify_matches(def_name, adts, ctrs, def_names, new_defs, match_count)
          .map_err(|err| MatchError::LetPat(Box::new(err)))?;
      }

      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.simplify_matches(def_name, adts, ctrs, def_names, new_defs, match_count)?;
        snd.simplify_matches(def_name, adts, ctrs, def_names, new_defs, match_count)?;
      }

      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }

    Ok(())
  }
}

/// Split each arm of a native number match on its own def and reconstructs the match term
fn match_native(
  scrutinee: Name,
  arms: Vec<(Pattern, Term)>,
  def_name: &Name,
  def_names: &mut DefNames,
  new_defs: &mut BTreeMap<DefId, Definition>,
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

    body = free_vars.iter().cloned().rev().fold(body, |acc, f| Term::Lam {
      tag: Tag::Static,
      nam: Some(f),
      bod: Box::new(acc),
    });

    if let Some(nam) = &bind {
      body = Term::Lam { tag: Tag::Static, nam: Some(nam.clone()), bod: Box::new(body) };
    }

    let name = make_def_name(def_name, &Name::new(name), match_count);
    let def_id = def_names.insert(name);
    let rules = vec![Rule { pats: Vec::new(), body }];
    let def = Definition { def_id, rules };
    new_defs.insert(def_id, def);

    let mut body = Term::Ref { def_id };

    if let Some(nam) = bind {
      body = Term::App { tag: Tag::Static, fun: Box::new(body), arg: Box::new(Term::Var { nam }) }
    }

    new_arms.push((rule, body));
  }

  free_vars
    .into_iter()
    .fold(Term::Match { scrutinee: Box::new(Term::Var { nam: scrutinee }), arms: new_arms }, |acc, nam| {
      Term::App { tag: Tag::Static, fun: Box::new(acc), arg: Box::new(Term::Var { nam }) }
    })
}

/// Transforms a match into a new definition with every arm of `arms` as a rule.
/// The result is the new def applied to the scrutinee followed by the free vars of the arms.
fn match_adt_app(
  scrutinee: Name,
  Adt { ctrs }: &Adt,
  arms: &[(Pattern, Term)],
  def_name: &Name,
  def_names: &mut DefNames,
  new_defs: &mut BTreeMap<DefId, Definition>,
  match_count: usize,
) -> Term {
  let mut rules = Vec::new();
  let mut free_vars = IndexSet::new();

  for (ctr_name, args) in ctrs {
    for (rule, term) in arms {
      let mut body = term.clone();

      let ctr = match rule {
        Pattern::Var(Some(ctr)) => ctr,
        Pattern::Ctr(ctr, pats) => {
          for (n, pat) in pats.iter().enumerate() {
            let Pattern::Var(Some(var)) = pat else { todo!() };
            body.subst(var, &Term::Var { nam: binded(&scrutinee, &args[n]) })
          }

          ctr
        }
        _ => unreachable!(),
      };

      if ctr == ctr_name {
        let pat = Pattern::Ctr(ctr_name.clone(), vec_name_to_pat(&scrutinee, args));
        let adt_binds: HashSet<_> = args.iter().map(|n| binded(&scrutinee, n)).collect();
        let term_free_vars = body.free_vars().into_keys().filter(|k| !adt_binds.contains(k));
        free_vars.extend(term_free_vars);

        let rule = Rule { pats: vec![pat], body };
        rules.push(rule);
      }
    }
  }

  for rule in &mut rules {
    for var in &free_vars {
      rule.pats.push(Pattern::Var(Some(var.clone())));
    }
  }

  let new_name = make_def_name(def_name, &Name::new("match"), match_count);
  let def_id = def_names.insert(new_name);
  let def = Definition { def_id, rules };
  new_defs.insert(def_id, def);

  let scrutinee_app = Term::App {
    tag: Tag::Static,
    fun: Box::new(Term::Ref { def_id }),
    arg: Box::new(Term::Var { nam: scrutinee }),
  };
  free_vars.into_iter().fold(scrutinee_app, |acc, nam| Term::App {
    tag: Tag::Static,
    fun: Box::new(acc),
    arg: Box::new(Term::Var { nam }),
  })
}

fn vec_name_to_pat(scrutinee: &Name, names: &[Name]) -> Vec<Pattern> {
  names.iter().map(|name| Pattern::Var(Some(binded(scrutinee, name)))).collect()
}

fn binded(bind: &Name, acc: &Name) -> Name {
  Name(format!("{bind}.{acc}"))
}

fn make_def_name(def_name: &Name, ctr: &Name, i: usize) -> Name {
  Name(format!("{def_name}${ctr}${i}"))
}
