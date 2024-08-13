use super::resolver::resolve;
use super::{repo, Error};

/// Ensures that downloaded packages matches the `mod.lock` file.
pub fn tidy() -> Result<(), Box<dyn Error>> {
  let (resolved, _) = resolve()?;

  for (name, versions) in resolved {
    for version in versions {
      let _ = repo::clone(&name, &version.to_string())?;
    }
  }

  Ok(())
}
