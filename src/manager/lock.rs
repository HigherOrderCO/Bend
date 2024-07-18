use super::save_config;
use semver::Version;
use std::{collections::BTreeMap, error::Error};
use toml_edit::{value, ArrayOfTables, DocumentMut, Table};

pub const LOCK_FILE: &str = "mod.lock";

const ARRAY_NAME: &str = "package";
pub const NAM_FIELD: &str = "name";
pub const VER_FIELD: &str = "version";

pub fn update_lock_file(resolved: BTreeMap<String, Version>) -> Result<(), Box<dyn Error>> {
  let mut packages = ArrayOfTables::new();

  for (name, version) in resolved {
    let mut table = Table::new();
    table[NAM_FIELD] = value(name);
    table[VER_FIELD] = value(version.to_string());
    packages.push(table);
  }

  let mut lock = DocumentMut::new();
  lock[ARRAY_NAME] = toml_edit::Item::ArrayOfTables(packages);
  save_config(lock, LOCK_FILE)
}

pub fn get_packages(config: &mut DocumentMut) -> Result<&mut ArrayOfTables, String> {
  config
    .entry(ARRAY_NAME)
    .or_insert_with(|| toml_edit::Item::ArrayOfTables(ArrayOfTables::new()))
    .as_array_of_tables_mut()
    .ok_or_else(|| format!("invalid '{LOCK_FILE}' format"))
}
