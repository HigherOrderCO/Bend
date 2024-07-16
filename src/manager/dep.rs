use super::config::{get_deps, insert, CONFIG_FILE};
use super::{get_config, repo, save_config};
use semver::VersionReq;
use std::error::Error;

/// Udates the module configuration file with the dependency information.
pub fn add(name: &str, version: Option<VersionReq>, alias: Option<&str>) -> Result<(), Box<dyn Error>> {
  let tag = match version {
    Some(req) => req.to_string(),
    None => repo::get_latest_tag(name, version)?.to_string(),
  };

  insert(name, &tag, alias)
}

/// Removes the dependency information from the module configuration file.
pub fn remove(name: &str) -> Result<(), Box<dyn Error>> {
  let mut config = get_config(CONFIG_FILE)?;
  let deps = get_deps(&mut config)?;

  if deps.remove(name).is_none() {
    return Err(format!("Dependency '{}' not found", name).into());
  }

  save_config(config, CONFIG_FILE)?;
  repo::remove(name)
}
