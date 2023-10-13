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
  pub fn check_self_referential_defs(&self) -> anyhow::Result<()> {
    for def in &self.defs {
      if let Term::Ref { def_id } = def.body {
        if def_id == def.def_id {
          return Err(anyhow::anyhow!(
            "Definition {} is self-referential",
            self.def_names.name(&def.def_id).unwrap()
          ));
        }
      }
    }
    Ok(())
  }
}
