use crate::term::{Book, DefId, DefNames, Name};

impl Book {
  pub fn check_has_main(&self) -> Result<DefId, String> {
    match (
      self.get_def(&Name::new(DefNames::ENTRY_POINT)),
      self.get_def(&Name::new(DefNames::HVM1_ENTRY_POINT)),
    ) {
      (None, None) => Err("File has no 'main' definition".to_string()),
      (Some(_), Some(_)) => Err("File has both 'Main' and 'main' definitions".to_string()),
      (None, Some(main)) | (Some(main), None) => {
        if main.rules.len() > 1 {
          Err("Main definition can't have more than one rule".to_string())
        } else if !main.rules[0].pats.is_empty() {
          Err("Main definition can't have any arguments".to_string())
        } else {
          Ok(main.def_id)
        }
      }
    }
  }
}
