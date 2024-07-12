use super::*;
use git2::{FetchOptions, Repository};
use semver::Version;
use std::{error::Error, path::Path};

/// Clones or updates a Git repository, checks out a specific version (if provided),
/// and updates the module configuration file with the dependency information.
pub fn get(name: &str, version: Option<String>, alias: Option<String>) -> Result<(), Box<dyn Error>> {
  let url = format!("https://{name}.git");

  let repo_name = alias.as_deref().unwrap_or_else(|| repository_name(name));
  let folder = format!(".bend/{}", repo_name);
  let local_path = Path::new(&folder);

  let tag = setup_repo(local_path, &url, version)?;

  update_mod(name, &tag, alias)
}

/// Sets up the repository at the given local path, cloning it if it doesn't exist,
/// and checks out the specified version or the latest tag.
fn setup_repo(local_path: &Path, url: &str, version: Option<String>) -> Result<String, Box<dyn Error>> {
  // Check if the repository already exists
  let repo = match Repository::open(local_path) {
    Ok(repo) => repo,
    Err(_) => Repository::init(local_path)?,
  };

  setup_remote(&repo, url, "origin")?;

  // Determine the tag to checkout
  let tag = match version {
    Some(ver) => ver,
    None => get_latest_tag(&repo)?,
  };

  // Checkout the specified tag
  if let Err(err) = checkout_tag(&repo, &tag) {
    match err.class() {
      git2::ErrorClass::Reference => return Err(format!("Version '{tag}' not found on '{url}'").into()),
      _ => return Err(err.message().into()),
    }
  }

  Ok(tag)
}

/// Sets up the remote URL for the repository, updating it if necessary,
/// and fetches all tags.
fn setup_remote(repo: &Repository, url: &str, remote_name: &str) -> Result<(), Box<dyn Error>> {
  let remote = match repo.find_remote(remote_name) {
    Ok(remote) if remote.url() != Some(url) => {
      repo.remote_set_url(remote_name, url)?;
      repo.find_remote(remote_name)?
    }
    Ok(remote) => remote,
    Err(_) => repo.remote(remote_name, url)?,
  };

  refresh_tags(repo, remote)
}

/// Refreshes the tags for the repository by deleting local tags and fetching remote tags.
fn refresh_tags(repo: &Repository, mut remote: git2::Remote) -> Result<(), Box<dyn Error>> {
  delete_local_tags(repo)?;
  let mut fetch_opts = FetchOptions::new();
  remote.fetch(&["refs/tags/*:refs/tags/*"], Some(&mut fetch_opts), None)?;
  Ok(())
}

/// Deletes all local tags from the repository.
fn delete_local_tags(repo: &Repository) -> Result<(), git2::Error> {
  let tags = repo.tag_names(None)?;
  for tag in tags.iter().flatten() {
    repo.tag_delete(tag)?;
  }
  Ok(())
}

/// Retrieves the latest tag from the repository by parsing the tag names as versions
/// and returning the highest version.
fn get_latest_tag(repo: &Repository) -> Result<String, Box<dyn Error>> {
  let refs = repo.references()?;
  let mut latest_tag: Option<Version> = None;

  for reference in refs {
    let reference = reference?;
    if reference.is_tag() {
      if let Some(tag) = reference.shorthand() {
        if let Ok(version) = Version::parse(tag) {
          if latest_tag.as_ref().map_or(true, |latest| version > *latest) {
            latest_tag = Some(version);
          }
        }
      }
    }
  }

  latest_tag.map(|v| v.to_string()).ok_or_else(|| "No tags found".into())
}

/// Checks out the specified tag in the repository, updating the HEAD to point to the tag's commit.
fn checkout_tag(repo: &Repository, tag: &str) -> Result<(), git2::Error> {
  // Find the commit corresponding to the tag
  let (object, reference) = repo.revparse_ext(tag)?;
  repo.checkout_tree(&object, None)?;

  // Update the HEAD to point to the tag's commit
  if let Some(ref_) = reference {
    repo.set_head(ref_.name().unwrap())?;
  } else {
    repo.set_head_detached(object.id())?;
  }

  Ok(())
}

/// Updates the module configuration file with the dependency information.
fn update_mod(name: &str, version: &str, alias: Option<String>) -> Result<(), Box<dyn Error>> {
  let mut config = get_config()?;
  let deps = get_deps(&mut config)?;

  match deps.get_mut(name) {
    Some(dep_item) => update_existing_dependency(dep_item, version, alias),
    None => _ = deps.insert(name, new_dependency(version, alias)),
  }

  save_config(config)
}

/// Updates an existing dependency with the new version and alias (if provided).
fn update_existing_dependency(dep_item: &mut Item, version: &str, alias: Option<String>) {
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
fn new_dependency(version: &str, alias: Option<String>) -> Item {
  if let Some(alias) = alias {
    let mut dep_table = Table::new();
    dep_table["version"] = value(version);
    dep_table["alias"] = value(alias);
    value(dep_table.into_inline_table())
  } else {
    value(version)
  }
}
