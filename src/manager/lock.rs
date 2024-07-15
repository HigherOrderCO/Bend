use std::error::Error;
use toml_edit::{value, ArrayOfTables, DocumentMut, Table};

pub const LOCK_FILE: &str = "mod.lock";

const ARRAY_NAME: &str = "package";
pub const NAM_FIELD: &str = "name";
pub const VER_FIELD: &str = "version";
pub const SRC_FIELD: &str = "source";

pub fn resolve() -> Result<(), Box<dyn Error>> {
  todo!() // Todo: resolve the versions of the mod.toml and all dependencies, adding to the lockfile
}

pub fn insert_table(
  config: &mut DocumentMut,
  name: &str,
  version: &str,
  source: &str,
) -> Result<(), Box<dyn Error>> {
  let packages = get_packages(config)?;

  let dep = packages.iter_mut().find(|dep| dep[NAM_FIELD].as_str() == Some(name));

  let dep = match dep {
    Some(dep) => dep,
    None => {
      let mut new_dep = Table::new();
      new_dep[NAM_FIELD] = value(name);

      let index = packages.len();
      packages.push(new_dep);
      packages.get_mut(index).unwrap()
    }
  };

  dep[VER_FIELD] = value(version);
  dep[SRC_FIELD] = value(source);

  Ok(())
}

pub fn get_packages(config: &mut DocumentMut) -> Result<&mut ArrayOfTables, String> {
  config
    .entry(ARRAY_NAME)
    .or_insert_with(|| toml_edit::Item::ArrayOfTables(ArrayOfTables::new()))
    .as_array_of_tables_mut()
    .ok_or_else(|| format!("invalid '{LOCK_FILE}' format"))
}
