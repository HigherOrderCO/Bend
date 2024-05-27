use crate::fun::{Ctx, Name};
use indexmap::IndexMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct RepeatedTopLevelNameErr {
  kind_fst: NameKind,
  kind_snd: NameKind,
  name: Name,
}

impl Ctx<'_> {
  /// Checks if there are any repeated top level names. Constructors
  /// and functions can't share names and adts can't share names.
  pub fn check_shared_names(&mut self) {
    let mut names = NameInfo::default();

    for adt_name in self.book.adts.keys() {
      names.add_name(adt_name, NameKind::Adt);
    }

    for ctr_name in self.book.ctrs.keys() {
      names.add_name(ctr_name, NameKind::Ctr);
    }

    for def_name in self.book.defs.keys() {
      names.add_name(def_name, NameKind::Def);
    }

    for err in names.into_errs() {
      self.info.add_book_error(err);
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum NameKind {
  Adt,
  Def,
  Ctr,
}

#[derive(Debug, Default)]
struct NameInfo<'a>(IndexMap<&'a Name, Vec<NameKind>>);

impl<'a> NameInfo<'a> {
  fn add_name(&mut self, name: &'a Name, kind: NameKind) {
    self.0.entry(name).or_default().push(kind);
  }

  fn into_errs(self) -> Vec<RepeatedTopLevelNameErr> {
    let mut errs = vec![];
    for (name, kinds) in self.0 {
      let mut num_adts = 0;
      let mut fst_ctr_def = None;
      for kind in kinds {
        if let NameKind::Adt = kind {
          num_adts += 1;
          if num_adts >= 2 {
            errs.push(RepeatedTopLevelNameErr {
              kind_fst: NameKind::Adt,
              kind_snd: NameKind::Adt,
              name: name.clone(),
            });
          }
        } else if let Some(fst) = fst_ctr_def {
          errs.push(RepeatedTopLevelNameErr { kind_fst: fst, kind_snd: kind, name: name.clone() });
        } else {
          fst_ctr_def = Some(kind);
        }
      }
    }
    errs
  }
}

impl Display for NameKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      NameKind::Adt => write!(f, "data type"),
      NameKind::Def => write!(f, "function"),
      NameKind::Ctr => write!(f, "constructor"),
    }
  }
}

impl std::fmt::Display for RepeatedTopLevelNameErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut snd = self.kind_snd.to_string();
    snd[0..1].make_ascii_uppercase();
    write!(f, "{} '{}' has the same name as a previously defined {}", snd, self.name, self.kind_fst)
  }
}
