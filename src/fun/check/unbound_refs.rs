use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Definitions, Name, Term},
  maybe_grow,
};
use std::collections::HashSet;

impl Ctx<'_> {
  pub fn check_unbound_refs(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();
    for def in self.book.defs.values() {
      let mut unbounds = HashSet::new();
      for rule in def.rules.iter() {
        rule.body.check_unbound_refs(&self.book.defs, &mut unbounds);
      }
      for unbound in unbounds {
        self.info.add_rule_error(format!("Reference to undefined function '{unbound}'"), def.name.clone());
      }
    }
    self.info.fatal(())
  }
}

impl Term {
  pub fn check_unbound_refs(&self, defs: &Definitions, unbounds: &mut HashSet<Name>) {
    maybe_grow(|| {
      if let Term::Ref { nam } = self {
        if !defs.contains_key(nam) {
          unbounds.insert(nam.clone());
        }
      }
      for child in self.children() {
        child.check_unbound_refs(defs, unbounds);
      }
    })
  }
}
