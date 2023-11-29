use crate::term::{Book, Name, RulePat};

impl Book {
  /// Resolve Constructor names inside rule patterns.
  /// When parsing a rule we don't have all the constructors yet,
  /// so no way to know if a particular name belongs to a constructor or is a matched variable.
  /// Therefore we must do it later, here.
  pub fn resolve_ctrs_in_pats(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        for pat in &mut rule.pats {
          pat.resolve_ctrs(&|nam| self.ctrs.contains_key(nam));
        }
      }
    }
  }
}

impl RulePat {
  pub fn resolve_ctrs(&mut self, is_ctr: &impl Fn(&Name) -> bool) {
    match self {
      RulePat::Var(nam) => {
        if is_ctr(nam) {
          *self = RulePat::Ctr(nam.clone(), vec![])
        }
      }
      RulePat::Ctr(_, args) => {
        for arg in args {
          arg.resolve_ctrs(is_ctr);
        }
      }
    }
  }
}
