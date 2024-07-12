use super::*;
use std::error::Error;
use std::path::Path;

pub fn remove(name: &str) -> Result<(), Box<dyn Error>> {
  remove_dep(name)?;
  remove_repo(name)
}

fn remove_dep(name: &str) -> Result<(), Box<dyn Error>> {
  let mut config = get_config()?;
  let deps = get_deps(&mut config)?;

  if deps.remove(name).is_none() {
    return Err(format!("Dependency '{}' not found", name).into());
  }

  save_config(config)
}

fn remove_repo(name: &str) -> Result<(), Box<dyn Error>> {
  let repo_name = repository_name(name);
  let folder = format!(".bend/{}", repo_name);
  let local_path = Path::new(&folder);

  if local_path.exists() {
    std::fs::remove_dir_all(local_path)?;
  }

  Ok(())
}
