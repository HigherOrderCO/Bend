use crate::term::{Book, Pattern};

impl Book {
  /// Resolve Constructor names inside rule patterns.
  /// When parsing a rule we don't have all the constructors yet,
  /// so no way to know if a particular name belongs to a constructor or is a matched variable.
  /// Therefore we must do it later, here.
  pub fn resolve_ctrs_in_pats(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        for pat in &mut rule.pats {
          if let Pattern::Var(nam) = &pat {
            if self.ctrs.contains_key(nam) {
              *pat = Pattern::Ctr(nam.clone(), vec![])
            }
          }
        }
      }
    }
  }
}
