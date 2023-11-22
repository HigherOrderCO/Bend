use crate::term::{
  check::type_check::{infer_arg_type, Type},
  Adt, Book, Name, RulePat, Term,
};
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  pub fn check_matches(&mut self) -> Result<(), String> {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.check_matches(&self.adts, &self.ctrs)?;
      }
    }
    Ok(())
  }

  pub fn simplify_matches(&mut self) -> Result<(), String> {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.simplify_matches(&self.adts)?;
      }
    }
    Ok(())
  }
}

#[allow(unused_variables)]
impl Term {
  pub fn check_matches(
    &mut self,
    adts: &BTreeMap<Name, Adt>,
    ctrs: &HashMap<Name, Name>,
  ) -> Result<(), String> {
    let check = |term: &mut Term, adts: &BTreeMap<Name, Adt>| {
      let Term::Match { scrutinee, arms } = term else { unreachable!() };

      let pats: Vec<_> = arms
        .iter()
        .map(|(rule, _)| match rule {
          RulePat::Var(nam) => RulePat::Ctr(nam.clone(), Vec::new()),
          _ => unreachable!(),
        })
        .collect();

      let ty = infer_arg_type(pats.iter(), ctrs).unwrap_or_else(|err| panic!("{err}"));

      let Type::Adt(nam) = ty else { unreachable!() };

      let Adt { ctrs } = &adts[&nam];

      let mut result = true;
      let mut names = HashSet::new();
      let mut repeated = HashSet::new();

      for (rule, _) in &mut *arms {
        let RulePat::Var(nam) = rule else { unreachable!() };

        if !names.insert(nam.clone()) {
          repeated.insert(nam.clone());
        }

        result &= ctrs.contains_key(nam);
      }

      if !repeated.is_empty() {
        return Err("Repeated these constructos: ".to_string()); // TODO: Better error
      }

      if arms.len() != ctrs.len() {
        return Err("Missing constructos: ".to_owned()); //TODO: Better error
      }

      if !result {
        return Err("Some rule in the match does not exist on the type".to_string()); // TODO: Better error
      }

      Ok(())
    };

    self.go(adts, &check)
  }

  pub fn simplify_matches(&mut self, adts: &BTreeMap<Name, Adt>) -> Result<(), String> {
    fn simplify(term: &mut Term, adts: &BTreeMap<Name, Adt>) -> Result<(), String> {
      let Term::Match { scrutinee, arms } = std::mem::replace(term, Term::Era) else { unreachable!() };

      let Term::Var { nam } = *scrutinee else {
        unreachable!(); // the scrutinee of a match on adts should always be a var
      };

      *term = Term::make_match_app(nam, arms, adts);

      Ok(())
    }

    self.go(adts, &simplify)
  }

  fn go(
    &mut self,
    adts: &BTreeMap<Name, Adt>,
    f: &impl Fn(&mut Term, &BTreeMap<Name, Adt>) -> Result<(), String>,
  ) -> Result<(), String> {
    match self {
      Term::Match { scrutinee, arms } => {
        if arms.is_empty() {
          return Err("Empty match block found".to_string());
        }

        if matches!(arms[0], (RulePat::Num(_), _)) {
          scrutinee.go(adts, f)?;

          for (_, term) in arms {
            term.go(adts, f)?;
          }
        } else {
          f(self, adts)?;
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.go(adts, f)?,

      Term::App { fun: fst, arg: snd }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.go(adts, f)?;
        snd.go(adts, f)?;
      }

      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }

    Ok(())
  }

  #[allow(unused_mut)]
  fn make_match_app(nam: Name, arms: Vec<(RulePat, Term)>, adts: &BTreeMap<Name, Adt>) -> Self {
    let mut res = Term::Var { nam };

    for (rule, arm) in arms {
      todo!("[{:?} => {:?}]", rule, arm)
    }

    res
  }
}
