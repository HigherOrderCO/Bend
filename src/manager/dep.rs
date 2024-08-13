use super::config::{get_deps, get_version, insert, CONFIG_FILE};
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

  let Some(dep) = deps.remove(name) else {
    return Err(format!("dependency '{}' not found", name).into());
  };

  let version = get_version(&dep).ok_or_else(|| format!("invalid version format '{}'", dep))?;

  save_config(config, CONFIG_FILE)?;
  repo::remove(name, version)
}
