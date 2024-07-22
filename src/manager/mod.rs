use clap::Subcommand;
use semver::VersionReq;
use std::{error::Error, fs::OpenOptions, io::Write};
use toml_edit::DocumentMut;

pub mod config;
mod dep;
mod lock;
mod repo;
mod resolver;
mod tidy;

#[derive(Subcommand, Clone, Debug)]
pub enum PackageCmd {
  /// Initializes a bend module.
  Init {
    #[arg(help = "Name of the module to initialize")]
    name: String,
  },
  /// Adds a dependency.
  Add {
    #[arg(help = "Name of the dependency to add")]
    name: String,
    #[arg(help = "Version of the dependency")]
    version: Option<VersionReq>,
    #[arg(short = 'a', long, help = "Dependency alias")]
    alias: Option<String>,
  },
  /// Removes a dependency.
  Remove {
    #[arg(help = "Name of the dependency to remove")]
    name: String,
  },
  /// Ensures that downloaded packages matches the `mod.lock` file.
  Tidy,
}

pub fn handle_package_cmd(command: PackageCmd) -> Result<(), Box<dyn Error>> {
  match command {
    PackageCmd::Init { name } => config::init(&name),
    PackageCmd::Add { name, version, alias } => dep::add(&name, version, alias.as_deref()),
    PackageCmd::Remove { name } => dep::remove(&name),
    PackageCmd::Tidy => tidy::tidy(),
  }
}

fn get_config(file: &str) -> Result<DocumentMut, Box<dyn Error>> {
  let file = std::fs::read_to_string(file)?;
  file.parse::<DocumentMut>().map_err(|_| format!("invalid '{file}' format").into())
}

fn save_config(config: DocumentMut, file: &str) -> Result<(), Box<dyn Error>> {
  let mut f = OpenOptions::new().create(true).write(true).truncate(true).open(file)?;
  write!(f, "{}", config)?;
  Ok(())
}
