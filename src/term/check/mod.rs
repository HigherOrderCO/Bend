use super::{Book, DefId, DefNames, Name};

pub mod exhaustiveness;
pub mod shared_names;
pub mod type_check;
pub mod unbound_vars;

impl Book {
  pub fn check_has_main(&self) -> Result<DefId, String> {
    match (
      self.def_names.def_id(&Name::new(DefNames::ENTRY_POINT)),
      self.def_names.def_id(&Name::new(DefNames::HVM1_ENTRY_POINT)),
    ) {
      (None, None) => Err("File has no 'main' definition".to_string()),
      (Some(_), Some(_)) => Err("File has both 'Main' and 'main' definitions".to_string()),
      (None, Some(main)) | (Some(main), None) => Ok(main),
    }
  }
}
