use super::config::{get_deps, update_config, CONFIG_FILE};
use super::{get_config, repo, save_config};
use git2::Repository;
use semver::VersionReq;
use std::error::Error;

/// Udates the module configuration file with the dependency information.
pub fn add(name: &str, version: Option<VersionReq>, alias: Option<&str>) -> Result<(), Box<dyn Error>> {
  let (url, local_path) = repo::get_url_and_path(name, alias);

  let repo = match Repository::open(&local_path) {
    Ok(repo) => repo,
    Err(_) => Repository::init(&local_path)?,
  };

  let tag = match version {
    Some(req) => req.to_string(),
    None => {
      repo::setup_remote(&repo, &url, "origin")?;
      repo::get_latest_tag(&repo, version)?.to_string()
    }
  };

  update_config(name, &tag, alias)
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
