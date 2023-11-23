use crate::term::{
  check::type_check::{infer_arg_type, Type},
  Adt, Book, Name, RulePat, Term, Definition, DefId, DefNames, Rule,
};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  pub fn simplify_matches(&mut self) -> Result<(), String> {
    let mut new_rules = BTreeMap::<DefId, Definition>::new();
    for (def_id, def) in &mut self.defs {
      let def_name = self.def_names.name(def_id).unwrap().clone();
      for rule in def.rules.iter_mut() {
        rule.body.simplify_matches(&def_name, &self.adts, &self.ctrs, &mut self.def_names, &mut new_rules, 0)?;
      }
    }
    self.defs.append(&mut new_rules);
    Ok(())
  }
}

impl Term {
  pub fn check_matches<'a>(
    pats: &[RulePat],
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
      let RulePat::Ctr(nam, _) = rule else { unreachable!() };

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
    match_count: usize,
  ) -> Result<(), String> {
    match self {
      Term::Match { scrutinee, arms } => {
        if arms.is_empty() {
          return Err("Empty match block found".to_string());
        }

        if matches!(arms[0], (RulePat::Num(_), _)) {
          scrutinee.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count + 1)?;

          for (_, term) in arms {
            term.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count + 1)?;
          }
        } else {
          let Term::Match { scrutinee, arms } = std::mem::replace(self, Term::Era) else { unreachable!() };

          let Term::Var { nam } = *scrutinee else {
            unreachable!(); // the scrutinee of a match on adts should always be a var
          };

          let rules: Vec<_> = arms
            .iter()
            .map(|(rule, _)| match rule {
              RulePat::Var(nam) => RulePat::Ctr(nam.clone(), Vec::new()),
              _ => unreachable!(),
            })
            .collect();

          let adt = Term::check_matches(&rules, adts, ctrs)?;

          // let arms = arms
          //   .into_iter()
          //   .map(|(pat, body)| {
          //     (pat, Term::make_match_app(nam.clone(), arms, adt, def_name, def_names, new_rules, match_count))
          //   }).collect();

          *self = Term::make_match_app(nam, arms, adt, def_name, def_names, new_rules, match_count + 1);
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count + 1)?,

      Term::App { fun: fst, arg: snd }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count + 1)?;
        snd.simplify_matches(def_name, adts, ctrs, def_names, new_rules, match_count + 1)?;
      }

      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }

    Ok(())
  }

  fn make_match_app(
    nam: Name,
    arms: Vec<(RulePat, Term)>,
    Adt { ctrs }: &Adt,
    def_name: &Name,
    def_names: &mut DefNames,
    new_rules: &mut BTreeMap<DefId, Definition>,
    match_count: usize,
  ) -> Self {
    let mut apps = vec![];

    for (ctr_name, args) in ctrs {
      for (rule, term) in arms.iter() {
        let RulePat::Var(ctr) = rule else { unreachable!() };
        if ctr == ctr_name {
          let lam = args
            .iter()
            .rev()
            .fold(term.clone(), |t, n| Term::Lam { nam: Some(binded(&nam, n)), bod: Box::new(t) });
          let def_name = gen_def_name(def_name, ctr_name, match_count);
          apps.push((lam, def_name));
        }
      }
    }

    let mut refs_to_app = Vec::new();

    for (body, name) in apps {
      let rules = vec![Rule { pats: Vec::new(), body }];
      let gen_id = def_names.insert(name.clone());
      let def = Definition { def_id: gen_id, rules };
      new_rules.insert(gen_id, def);
      refs_to_app.push(gen_id);
    }

    refs_to_app
      .into_iter()
      .fold(Term::Var { nam }, |scrutinee, t| Term::App { fun: Box::new(scrutinee), arg: Box::new(Term::Ref { def_id: t }) })
  }
}

fn binded(bind: &Name, acc: &Name) -> Name {
  Name::new(&format!("{bind}.{acc}"))
}

fn gen_def_name(def_name: &Name, ctr: &Name, i: usize) -> Name {
  Name::new(&format!("{def_name}${ctr}${i}"))
}
