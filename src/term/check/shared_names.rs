use std::fmt::Display;

use indexmap::IndexMap;

use crate::{
  diagnostics,
  term::{Ctx, Name},
};

#[derive(Debug, Clone)]
pub struct TopLevelErr(Name);

impl Display for TopLevelErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Duplicated top-level name '{}'.", self.0)
  }
}

impl Ctx<'_> {
  /// Checks if exists shared names from definitions, adts and constructors, allowing constructors
  /// share the adt name once.
  pub fn check_shared_names(&mut self) {
    let mut checked = IndexMap::<&Name, NameInfo>::new();

    for adt_name in self.book.adts.keys() {
      checked.entry(adt_name).or_insert(NameInfo::new(NameKind::Adt)).with_adt(adt_name, &mut self.info);
    }

    for ctr_name in self.book.ctrs.keys() {
      checked.entry(ctr_name).or_insert(NameInfo::new(NameKind::Ctr)).with_ctr(ctr_name, &mut self.info);
    }

    for def_name in self.book.defs.keys() {
      checked.entry(def_name).or_insert(NameInfo::new(NameKind::Def)).with_def(def_name, &mut self.info);
    }

    for (name, name_info) in checked.into_iter() {
      if name_info.count > 1 {
        self.info.error(TopLevelErr(name.clone()));
      }
    }
  }
}

#[derive(Debug)]
enum NameKind {
  Adt,
  Def,
  Ctr,
}

#[derive(Debug)]
struct NameInfo {
  kind: NameKind,
  count: usize,
}

impl NameInfo {
  fn new(kind: NameKind) -> Self {
    Self { kind, count: 0 }
  }
}

impl NameInfo {
  fn with_ctr(&mut self, current_name: &Name, info: &mut diagnostics::Info) {
    match self.kind {
      NameKind::Adt => {} // Error caught by the parser
      NameKind::Def => info.error(TopLevelErr(current_name.clone())),
      NameKind::Ctr => {} // Error caught by the parser
    }
  }

  fn with_def(&mut self, current_name: &Name, info: &mut diagnostics::Info) {
    match self.kind {
      NameKind::Adt => self.count += 1,
      NameKind::Def => {}
      NameKind::Ctr => info.error(TopLevelErr(current_name.clone())),
    }
  }

  fn with_adt(&mut self, current_name: &Name, info: &mut diagnostics::Info) {
    match self.kind {
      NameKind::Adt => self.count += 1,
      NameKind::Def => info.error(TopLevelErr(current_name.clone())),
      NameKind::Ctr => self.count += 1,
    }
  }
}
