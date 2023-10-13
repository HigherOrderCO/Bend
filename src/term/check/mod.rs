use super::{DefId, DefinitionBook, Name, Term};

pub mod unbound_vars;

impl DefinitionBook {
  pub fn check_has_main(&self) -> anyhow::Result<DefId> {
    if let Some(main) = self.def_names.def_id(&Name::new("Main")) {
      Ok(main)
    } else {
      Err(anyhow::anyhow!("File has no 'Main' definition"))
    }
  }

  /// Check that a definition is not just a reference to itself
  pub fn check_ref_to_ref(&self) -> anyhow::Result<()> {
    for (_, def) in &self.defs {
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
