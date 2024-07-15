use git2::{FetchOptions, Repository};
use semver::{Version, VersionReq};
use std::error::Error;
use std::path::Path;
use std::path::PathBuf;

// Clones or updates a dep Git repository, checks out a specific version (if provided)
pub fn clone(name: &str, tag: &str, alias: Option<&str>) -> Result<(), Box<dyn Error>> {
  let (url, local_path) = get_url_and_path(name, alias);

  let repo = match Repository::open(&local_path) {
    Ok(repo) => repo,
    Err(_) => Repository::init(&local_path)?,
  };

  setup_remote(&repo, &url, "origin")?;

  // Checkout the specified tag
  if let Err(err) = checkout_tag(&repo, tag) {
    match err.class() {
      git2::ErrorClass::Reference => return Err(format!("Version '{tag}' not found on '{url}'").into()),
      _ => return Err(err.message().into()),
    }
  }

  Ok(())
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

/// Removes the local repository of the given package
pub fn remove(name: &str) -> Result<(), Box<dyn Error>> {
  let repo_name = repository_name(name);
  let folder = format!(".bend/{}", repo_name);
  let local_path = Path::new(&folder);

  if local_path.exists() {
    std::fs::remove_dir_all(local_path)?;
  }

  Ok(())
}

pub fn get_url_and_path(name: &str, alias: Option<&str>) -> (String, PathBuf) {
  let url = format!("https://{name}.git");

  let repo_name = alias.unwrap_or_else(|| repository_name(name));
  let folder = format!(".bend/{}", repo_name);
  let local_path = PathBuf::from(folder);

  (url, local_path)
}

/// Extracts the repository name from a full repository URL.
/// Assumes the URL is in the format `user/repo`.
fn repository_name(name: &str) -> &str {
  let (_user, repo) = name.rsplit_once('/').expect("Invalid repository URL");
  repo
}

/// Sets up the remote URL for the repository, updating it if necessary,
/// and fetches all tags.
pub fn setup_remote(repo: &Repository, url: &str, remote_name: &str) -> Result<(), Box<dyn Error>> {
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
pub fn get_latest_tag(repo: &Repository, constraint: Option<VersionReq>) -> Result<Version, Box<dyn Error>> {
  let refs = repo.references()?;
  let mut latest: Option<Version> = None;

  for reference in refs {
    let reference = reference?;
    if reference.is_tag() {
      if let Some(tag) = reference.shorthand() {
        if let Ok(version) = Version::parse(tag) {
          let is_within_constraint = match &constraint {
            Some(req) => req.matches(&version),
            None => true,
          };
          if is_within_constraint && latest.as_ref().map_or(true, |latest| version > *latest) {
            latest = Some(version);
          }
        }
      }
    }
  }

  latest.ok_or_else(|| "No tags found".into())
}
