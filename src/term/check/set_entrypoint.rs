use crate::{
  term::{Book, Ctx, Definition, Name},
  ENTRY_POINT, HVM1_ENTRY_POINT,
};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum EntryErr {
  NotFound(Name),
  Multiple(Vec<Name>),
  MultipleRules,
}

impl Display for EntryErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      EntryErr::NotFound(name) => write!(f, "File has no '{name}' definition."),
      EntryErr::Multiple(fnd) if fnd.len() == 2 => {
        write!(f, "File has both '{}' and '{}' definitions.", fnd[0], fnd[1])
      }
      EntryErr::Multiple(fnd) => {
        write!(f, "File has '{}', '{}' and '{}' definitions.", fnd[0], fnd[1], fnd[2])
      }
      EntryErr::MultipleRules => write!(f, "Main definition can't have more than one rule."),
    }
  }
}

impl Ctx<'_> {
  pub fn set_entrypoint(&mut self) {
    // already setted
    if self.book.entrypoint.is_some() {
      return;
    }

    let mut entrypoint = None;

    let (custom, main, hvm1_main) = self.book.get_possible_entry_points();
    match (custom, main, hvm1_main) {
      (Some(entry), None, None) | (None, Some(entry), None) | (None, None, Some(entry)) => {
        match validate_entry_point(entry) {
          Ok(name) => entrypoint = Some(name),
          Err(err) => self.info.error(err),
        }
      }

      (Some(a), Some(b), None) | (None, Some(a), Some(b)) | (Some(a), None, Some(b)) => {
        self.info.error(EntryErr::Multiple(vec![a.name.clone(), b.name.clone()]));

        match validate_entry_point(a) {
          Ok(name) => entrypoint = Some(name),
          Err(err) => self.info.error(err),
        }
      }

      (Some(a), Some(b), Some(c)) => {
        self.info.error(EntryErr::Multiple(vec![a.name.clone(), b.name.clone(), c.name.clone()]));

        match validate_entry_point(a) {
          Ok(name) => entrypoint = Some(name),
          Err(err) => self.info.error(err),
        }
      }

      (None, None, None) => {
        self.info.error(EntryErr::NotFound(self.book.entrypoint.clone().unwrap_or(Name::from(ENTRY_POINT))))
      }
    }

    self.book.entrypoint = entrypoint;
  }
}

fn validate_entry_point(entry: &Definition) -> Result<Name, EntryErr> {
  if entry.rules.len() > 1 { Err(EntryErr::MultipleRules) } else { Ok(entry.name.clone()) }
}

impl Book {
  fn get_possible_entry_points(&self) -> (Option<&Definition>, Option<&Definition>, Option<&Definition>) {
    let custom = self.entrypoint.as_ref().and_then(|e| self.defs.get(e));
    let main = self.defs.get(&Name::from(ENTRY_POINT));
    let hvm1_main = self.defs.get(&Name::from(HVM1_ENTRY_POINT));
    (custom, main, hvm1_main)
  }
}
