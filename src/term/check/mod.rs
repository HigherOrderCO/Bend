use super::{DefId, DefNames, DefinitionBook, Name, Term};

pub mod unbound_vars;

impl DefinitionBook {
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

  /// Check that a definition is not just a reference to itself
  pub fn check_ref_to_ref(&self) -> anyhow::Result<()> {
    for def in self.defs.values() {
      if let Term::Ref { .. } = def.body {
        return Err(anyhow::anyhow!(
          "Definition {} is just a reference to another definition",
          self.def_names.name(&def.def_id).unwrap()
        ));
      }
    }
    Ok(())
  }
}
