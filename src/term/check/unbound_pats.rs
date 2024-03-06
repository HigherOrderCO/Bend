use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Pattern, Term},
};
use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone)]
pub struct UnboundCtrErr(Name);

impl Display for UnboundCtrErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Unbound constructor '{}'.", self.0)
  }
}

impl Ctx<'_> {
  /// Check if the constructors in rule patterns or match patterns are defined.
  pub fn check_unbound_pats(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    let is_ctr = |nam: &Name| self.book.ctrs.contains_key(nam);
    for (def_name, def) in self.book.defs.iter() {
      for rule in &def.rules {
        for pat in &rule.pats {
          let res = pat.check_unbounds(&is_ctr);
          self.info.take_err(res, Some(def_name));
        }

        let res = rule.body.check_unbound_pats(&is_ctr);
        self.info.take_err(res, Some(def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Pattern {
  pub fn check_unbounds(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtrErr> {
    let unbounds = self.unbound_pats(is_ctr);
    if let Some(unbound) = unbounds.iter().next() { Err(UnboundCtrErr(unbound.clone())) } else { Ok(()) }
  }

  /// Given a possibly nested rule pattern, return a set of all used but not declared constructors.
  pub fn unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> HashSet<Name> {
    let mut unbounds = HashSet::new();
    let mut check = vec![self];
    while let Some(pat) = check.pop() {
      if let Pattern::Ctr(nam, _) = pat {
        if !is_ctr(nam) {
          unbounds.insert(nam.clone());
        }
      }
      check.extend(pat.children());
    }
    unbounds
  }
}

impl Term {
  pub fn check_unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtrErr> {
    Term::recursive_call(move || {
      for pat in self.patterns() {
        pat.check_unbounds(is_ctr)?;
      }
      for child in self.children() {
        child.check_unbound_pats(is_ctr)?;
      }
      Ok(())
    })
  }
}
