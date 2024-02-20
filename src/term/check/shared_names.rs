use std::{collections::HashMap, fmt::Display};

use crate::{
  diagnostics::Error,
  term::{Book, Name},
};

#[derive(Debug, Clone)]
pub struct TopLevelErr(Name);

impl Display for TopLevelErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Duplicated top-level name '{}'", self.0)
  }
}

impl Book {
  /// Checks if exists shared names from definitions, adts and constructors.
  pub fn check_shared_names(&mut self) {
    let mut checked = HashMap::<&Name, usize>::new();

    for adt_name in self.adts.keys() {
      *checked.entry(adt_name).or_default() += 1;
    }

    for ctr_name in self.ctrs.keys() {
      *checked.entry(ctr_name).or_default() += 1;
    }

    for def_name in self.defs.keys() {
      *checked.entry(def_name).or_default() += 1;
    }

    for (name, n) in checked.into_iter() {
      if n > 1 {
        self.info.error(Error::TopLevel(TopLevelErr(name.clone())));
      }
    }
  }
}
