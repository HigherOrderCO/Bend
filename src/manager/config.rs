use super::{get_config, save_config};
use std::error::Error;
use toml_edit::{value, DocumentMut, Item, Table, TableLike};

pub const CONFIG_FILE: &str = "mod.toml";

/// Initializes a new module configuration file with the given module name.
pub fn init(name: &str) -> Result<(), Box<dyn Error>> {
  let mut config = DocumentMut::new();
  config["module"] = value(name);
  save_config(config, CONFIG_FILE)
}

pub fn get_deps(config: &mut DocumentMut) -> Result<&mut dyn TableLike, Box<dyn Error>> {
  config["dependencies"]
    .or_insert(Item::Table(Table::new()))
    .as_table_like_mut()
    .ok_or_else(|| format!("invalid '{CONFIG_FILE}' format").into())
}

/// Updates the module configuration file with the dependency information.
pub fn insert(name: &str, version: &str, alias: Option<&str>) -> Result<(), Box<dyn Error>> {
  let mut config = get_config(CONFIG_FILE)?;
  let deps = get_deps(&mut config)?;

  match deps.get_mut(name) {
    Some(dep_item) => update_existing_dependency(dep_item, version, alias),
    None => _ = deps.insert(name, new_dependency(version, alias)),
  }

  save_config(config, CONFIG_FILE)
}

/// Updates an existing dependency with the new version and alias (if provided).
fn update_existing_dependency(dep_item: &mut Item, version: &str, alias: Option<&str>) {
  match dep_item.as_table_like_mut() {
    None => *dep_item = new_dependency(version, alias),
    Some(table) => {
      table.insert("version", value(version));
      if let Some(alias) = alias {
        table.insert("alias", value(alias));
      } else {
        table.remove("alias");
      }
    }
  }
}

/// Creates a new dependency with the given version and alias (if provided).
fn new_dependency(version: &str, alias: Option<&str>) -> Item {
  if let Some(alias) = alias {
    let mut dep_table = Table::new();
    dep_table["version"] = value(version);
    dep_table["alias"] = value(alias);
    value(dep_table.into_inline_table())
  } else {
    value(version)
  }
}
