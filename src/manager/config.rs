use super::{get_config, save_config};
use std::{error::Error, fs::OpenOptions, io::Write, path::PathBuf};
use toml_edit::{table, value, DocumentMut, Item, Table, TableLike};

pub const CONFIG_FILE: &str = "mod.toml";

/// Initializes a new module configuration file with the given module name.
pub fn init(name: &str) -> Result<(), Box<dyn Error>> {
  // TODO: initialize a git repo? or just add a .gitignote?
  let main_file = "src/main.bend";
  let path = PathBuf::from(main_file);

  if !path.exists() {
    std::fs::create_dir("src")?;
    let mut f = OpenOptions::new().create_new(true).write(true).open(path)?;
    write!(f, "def main():\n  return \"Hello World!\"\n")?;
  }

  let mut config = DocumentMut::new();
  config["module"] = value(name);

  let mut table = table();
  table["path"] = value(main_file);

  config.insert("lib", table);
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

pub fn get_version(item: &Item) -> Option<&str> {
  if let Some(v) = item.as_str() {
    Some(v)
  } else {
    let tab = item.as_table_like()?;
    let ver = tab.get("version")?;
    ver.as_str()
  }
}

/// Gets the main path from a bend project config file,
/// used when a path is not provided when invoking the CLI.
pub fn get_project_path() -> Result<PathBuf, String> {
  let config = get_config(CONFIG_FILE).map_err(|_| {
    format!(
      "failed to read a valid '{CONFIG_FILE}', \
        pass the file path as an argument, \
        or make sure you are in a valid bend project"
    )
  })?;

  let path = config
    .get("lib")
    .and_then(|lib| lib.get("path"))
    .and_then(|path| path.as_str())
    .ok_or_else(|| format!("invalid '{CONFIG_FILE}' format"))?;

  Ok(PathBuf::from(path))
}
