use clap::Subcommand;
use std::{error::Error, fs::OpenOptions, io::Write};
use toml_edit::{value, DocumentMut, Item, Table, TableLike};

mod get;
mod remove;

const CONFIG_FILE: &str = "mod.toml";

#[derive(Subcommand, Clone, Debug)]
pub enum PackageCmd {
  /// Initializes a bend module
  Init {
    #[arg(help = "Name of the module to initialize")]
    name: String,
  },
  /// Adds a dependency
  Get {
    #[arg(help = "Name of the dependency to add")]
    name: String,
    #[arg(help = "Version of the dependency")]
    version: Option<String>,
    #[arg(short = 'a', long, help = "Dependency alias")]
    alias: Option<String>,
  },
  /// Removes a dependency
  Remove {
    #[arg(help = "Name of the dependency to remove")]
    name: String,
  },
  Tidy,
}

pub fn handle_package_cmd(command: PackageCmd) -> Result<(), Box<dyn Error>> {
  match command {
    PackageCmd::Init { name } => init(&name),
    PackageCmd::Get { name, version, alias } => get::get(&name, version, alias),
    PackageCmd::Remove { name } => remove::remove(&name),
    PackageCmd::Tidy {} => todo!(),
  }
}

/// Initializes a new module configuration file with the given module name.
fn init(name: &str) -> Result<(), Box<dyn Error>> {
  let mut config = DocumentMut::new();
  config["module"] = value(name);
  save_config(config)
}

/// Extracts the repository name from a full repository URL.
/// Assumes the URL is in the format `user/repo`.
fn repository_name(name: &str) -> &str {
  let (_user, repo) = name.rsplit_once('/').expect("Invalid repository URL");
  repo
}

fn get_config() -> Result<DocumentMut, Box<dyn Error>> {
  let file = std::fs::read_to_string(CONFIG_FILE)?;
  file.parse::<DocumentMut>().map_err(|_| format!("invalid '{CONFIG_FILE}' format").into())
}

fn save_config(config: DocumentMut) -> Result<(), Box<dyn Error>> {
  let mut f = OpenOptions::new().create(true).write(true).truncate(true).open(CONFIG_FILE)?;
  write!(f, "{}", config)?;
  Ok(())
}

fn get_deps(config: &mut DocumentMut) -> Result<&mut dyn TableLike, Box<dyn Error>> {
  config["dependencies"]
    .or_insert(Item::Table(Table::new()))
    .as_table_like_mut()
    .ok_or_else(|| "invalid 'mod.toml' format".into())
}
