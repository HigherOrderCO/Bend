use super::config::{get_deps, CONFIG_FILE};
use super::lock::{get_packages, LOCK_FILE, NAM_FIELD, VER_FIELD};
use super::resolver::resolve;
use super::{get_config, repo, Error};

/// Ensures that downloaded packages matches the `mod.lock` file.
pub fn tidy() -> Result<(), Box<dyn Error>> {
  resolve()?;
  clone_locked()
}

fn clone_locked() -> Result<(), Box<dyn Error>> {
  let mut config = get_config(CONFIG_FILE)?;
  let deps = get_deps(&mut config)?;

  let mut lock = get_config(LOCK_FILE)?;
  let pkgs = get_packages(&mut lock)?;

  for pkg in pkgs.iter() {
    let nam = pkg[NAM_FIELD].as_str().unwrap();
    let ver = pkg[VER_FIELD].as_str().unwrap();
    let alias = deps.get(nam).and_then(|dep| dep.get("alias")).and_then(|alias| alias.as_str());

    let _ = repo::clone(nam, ver, alias)?;
  }

  Ok(())
}
