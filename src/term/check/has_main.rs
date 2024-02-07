use crate::{
  term::{Book, DefName},
  ENTRY_POINT, HVM1_ENTRY_POINT,
};

impl Book {
  pub fn check_has_main(&self) -> Result<DefName, String> {
    match (self.defs.get(&DefName::new(ENTRY_POINT)), self.defs.get(&DefName::new(HVM1_ENTRY_POINT))) {
      (None, None) => Err("File has no 'main' definition".to_string()),
      (Some(_), Some(_)) => Err("File has both 'Main' and 'main' definitions".to_string()),
      (None, Some(main)) | (Some(main), None) => {
        if main.rules.len() > 1 {
          Err("Main definition can't have more than one rule".to_string())
        } else if !main.rules[0].pats.is_empty() {
          Err("Main definition can't have any arguments".to_string())
        } else {
          Ok(main.name.clone())
        }
      }
    }
  }
}
