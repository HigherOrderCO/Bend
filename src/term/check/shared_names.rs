use std::{collections::HashMap, fmt::Display};

use crate::term::{Ctx, Name};

#[derive(Debug, Clone)]
pub struct TopLevelErr(Name);

impl Display for TopLevelErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Duplicated top-level name '{}'.", self.0)
  }
}

impl Ctx {
  /// Checks if exists shared names from definitions, adts and constructors.
  pub fn check_shared_names(&mut self) {
    let mut checked = HashMap::<&Name, usize>::new();

    for adt_name in self.book.adts.keys() {
      *checked.entry(adt_name).or_default() += 1;
    }

    for ctr_name in self.book.ctrs.keys() {
      *checked.entry(ctr_name).or_default() += 1;
    }

    for def_name in self.book.defs.keys() {
      *checked.entry(def_name).or_default() += 1;
    }

    for (name, n) in checked.into_iter() {
      if n > 1 {
        self.info.error(TopLevelErr(name.clone()));
      }
    }
  }
}
