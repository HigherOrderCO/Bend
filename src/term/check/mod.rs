use super::{Book, DefId, DefNames, Name};

pub mod exhaustiveness;
pub mod shared_names;
pub mod type_check;
pub mod unbound_vars;

impl Book {
  pub fn check_has_main(&self) -> anyhow::Result<DefId> {
    match (
      self.def_names.def_id(&Name::new(DefNames::ENTRY_POINT)),
      self.def_names.def_id(&Name::new(DefNames::HVM1_ENTRY_POINT)),
    ) {
      (None, None) => Err(anyhow::anyhow!("File has no 'main' definition")),
      (Some(_), Some(_)) => Err(anyhow::anyhow!("File has both 'Main' and 'main' definitions")),
      (None, Some(main)) | (Some(main), None) => Ok(main),
    }
  }
}
