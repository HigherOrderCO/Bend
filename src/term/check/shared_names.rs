use std::collections::HashSet;

use crate::term::Book;

impl Book {
  /// Checks if exists shared names from definitions, adts and constructors.
  pub fn check_shared_names(&self) -> Result<(), String> {
    let mut checked = HashSet::new();

    for adt_name in self.adts.keys() {
      if !checked.insert(adt_name) {
        return Err(format!("Duplicated top-level name '{adt_name}'"));
      }
    }

    for ctr_name in self.ctrs.keys() {
      if !checked.insert(ctr_name) {
        return Err(format!("Duplicated top-level name '{ctr_name}'"));
      }
    }

    for def_name in self.defs.keys() {
      if !checked.insert(def_name) {
        return Err(format!("Duplicated top-level name '{def_name}'"));
      }
    }

    Ok(())
  }
}
