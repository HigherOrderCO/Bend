use crate::{
  diagnostics::Error,
  term::{Book, Name, Pattern, Term},
};
use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone)]
pub struct UnboundCtr(Name);

impl Display for UnboundCtr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Unbound constructor '{}'", self.0)
  }
}

impl Book {
  /// Check if the constructors in rule patterns or match patterns are defined.
  pub fn check_unbound_pats(&mut self) -> Result<(), String> {
    self.info.start_pass();

    let is_ctr = |nam: &Name| self.ctrs.contains_key(nam);
    for (def_name, def) in self.defs.iter() {
      for rule in &def.rules {
        for pat in &rule.pats {
          let res = pat.check_unbounds(&is_ctr);
          self.info.errs.extend(res.map_err(|e| Error::UnboundCtr(def_name.clone(), e)).err())
        }

        let res = rule.body.check_unbound_pats(&is_ctr);
        self.info.errs.extend(res.map_err(|e| Error::UnboundCtr(def_name.clone(), e)).err())
      }
    }

    self.info.fatal(())
  }
}

impl Pattern {
  pub fn check_unbounds(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtr> {
    let unbounds = self.unbound_pats(is_ctr);
    if let Some(unbound) = unbounds.iter().next() { Err(UnboundCtr(unbound.clone())) } else { Ok(()) }
  }

  /// Given a possibly nested rule pattern, return a set of all used but not declared constructors.
  pub fn unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> HashSet<Name> {
    let mut unbounds = HashSet::new();
    let mut check = vec![self];
    while let Some(pat) = check.pop() {
      match pat {
        Pattern::Ctr(nam, args) => {
          if !is_ctr(nam) {
            unbounds.insert(nam.clone());
          }
          check.extend(args.iter());
        }
        Pattern::Tup(fst, snd) => {
          check.push(fst);
          check.push(snd);
        }
        Pattern::Lst(args) => args.iter().for_each(|arg| check.push(arg)),
        Pattern::Var(_) | Pattern::Num(_) => {}
      }
    }
    unbounds
  }
}

impl Term {
  pub fn check_unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtr> {
    match self {
      Term::Let { pat, val, nxt } => {
        pat.check_unbounds(is_ctr)?;
        val.check_unbound_pats(is_ctr)?;
        nxt.check_unbound_pats(is_ctr)?;
      }
      Term::Mat { matched, arms } => {
        matched.check_unbound_pats(is_ctr)?;
        for (pat, body) in arms {
          pat.check_unbounds(is_ctr)?;
          body.check_unbound_pats(is_ctr)?;
        }
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.check_unbound_pats(is_ctr)?;
        snd.check_unbound_pats(is_ctr)?;
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.check_unbound_pats(is_ctr)?,
      Term::Lst { .. } => unreachable!(),
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Ref { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Era
      | Term::Err => (),
    }
    Ok(())
  }
}
