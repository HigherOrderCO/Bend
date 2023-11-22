use crate::term::{
  check::type_check::{infer_arg_type, Type},
  Adt, Book, Name, RulePat, Term,
};
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  pub fn simplify_matches(&mut self) -> Result<(), String> {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.simplify_matches(&self.adts, &self.ctrs)?;
      }
    }
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
      let plural_or_singular = ctrs_plural_or_sing(repeated.len());
      return Err(format!("Repeated {plural_or_singular} in a match block: {}", repeated.iter().join(", ")));
    }

    if !missing.is_empty() {
      let plural_or_singular = ctrs_plural_or_sing(missing.len());
      return Err(format!("Missing {plural_or_singular} in a match block: {}", missing.iter().join(", ")));
    }

    Ok(&adts[&nam])
  }

  pub fn simplify_matches(
    &mut self,
    adts: &BTreeMap<Name, Adt>,
    ctrs: &HashMap<Name, Name>,
  ) -> Result<(), String> {
    match self {
      Term::Match { scrutinee, arms } => {
        if arms.is_empty() {
          return Err("Empty match block found".to_string());
        }

        if matches!(arms[0], (RulePat::Num(_), _)) {
          scrutinee.simplify_matches(adts, ctrs)?;

          for (_, term) in arms {
            term.simplify_matches(adts, ctrs)?;
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

          *self = Term::make_match_app(nam, arms, adt);
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.simplify_matches(adts, ctrs)?,

      Term::App { fun: fst, arg: snd }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.simplify_matches(adts, ctrs)?;
        snd.simplify_matches(adts, ctrs)?;
      }

      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }

    Ok(())
  }

  fn make_match_app(nam: Name, arms: Vec<(RulePat, Term)>, Adt { ctrs }: &Adt) -> Self {
    let mut apps = vec![];

    for (ctr_name, args) in ctrs {
      for (rule, term) in &arms {
        let RulePat::Var(ctr) = rule else { unreachable!() };
        if ctr == ctr_name {
          let lam = args
            .iter()
            .rev()
            .fold(term.clone(), |t, n| Term::Lam { nam: Some(binded(&nam, n)), bod: Box::new(t) });
          apps.push(lam);
        }
      }
    }

    apps
      .into_iter()
      .fold(Term::Var { nam }, |scrutinee, t| Term::App { fun: Box::new(scrutinee), arg: Box::new(t) })
  }
}

fn binded(bind: &Name, acc: &Name) -> Name {
  Name::new(&format!("{bind}.{acc}"))
}
