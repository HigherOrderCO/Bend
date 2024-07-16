use git2::{AutotagOption, Remote, RemoteCallbacks};
use git2::{FetchOptions, Repository};
use semver::{Version, VersionReq};
use std::error::Error;
use std::path::Path;
use std::path::PathBuf;

const DEPS_FOLDER: &str = ".bend";

// Clones or updates a dep Git repository, checks out a specific version (if provided)
pub fn clone(name: &str, tag: &str, alias: Option<&str>) -> Result<PathBuf, Box<dyn Error>> {
  let repo_name = alias.unwrap_or_else(|| repository_name(name));
  let folder = format!("{DEPS_FOLDER}/{repo_name}");
  let local_path = PathBuf::from(folder);

  let repo = match Repository::open(&local_path) {
    Ok(repo) => repo,
    Err(_) => Repository::init(&local_path)?,
  };

  setup_remote(&repo, name, "origin")?;

  // Checkout the specified tag
  if let Err(err) = checkout_tag(&repo, tag) {
    match err.class() {
      git2::ErrorClass::Reference => return Err(format!("Version '{tag}' not found on '{name}'").into()),
      _ => return Err(err.message().into()),
    }
  }

  Ok(local_path)
}

/// Checks out the specified tag in the repository, updating the HEAD to point to the tag's commit.
fn checkout_tag(repo: &Repository, tag: &str) -> Result<(), git2::Error> {
  // Find the commit corresponding to the tag
  let (object, reference) = repo.revparse_ext(tag)?;
  repo.checkout_tree(&object, None)?;

  // Update the HEAD to point to the tag's commit
  match reference {
    Some(gref) => repo.set_head(gref.name().unwrap()),
    None => repo.set_head_detached(object.id()),
  }
}

/// Removes the local repository of the given package
pub fn remove(name: &str) -> Result<(), Box<dyn Error>> {
  let repo_name = repository_name(name);
  let folder = format!("{DEPS_FOLDER}/{repo_name}");
  let local_path = Path::new(&folder);

  if local_path.exists() {
    std::fs::remove_dir_all(local_path)?;
  }

  Ok(())
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
  let url = &format!("https://{url}.git");

  let mut remote = match repo.find_remote(remote_name) {
    Ok(remote) if remote.url() != Some(url) => {
      repo.remote_set_url(remote_name, url)?;
      repo.find_remote(remote_name)?
    }
    Ok(remote) => remote,
    Err(_) => repo.remote(remote_name, url)?,
  };

  fetch_tags(&mut remote)?;
  Ok(())
}

pub fn get_remote_tags(url: &str) -> Result<Vec<String>, Box<dyn Error>> {
  let url = &format!("https://{url}.git");

  let temp_dir = tempfile::tempdir()?;
  let repo_path = temp_dir.path();

  let repo = Repository::init(repo_path)?;
  let mut remote = repo.remote("origin", url)?;
  fetch_tags(&mut remote)?;

  let tags: Vec<String> = repo.tag_names(None)?.iter().filter_map(|t| t.map(String::from)).collect();

  Ok(tags)
}

fn fetch_tags(remote: &mut Remote) -> Result<(), git2::Error> {
  let callbacks = RemoteCallbacks::new();
  let mut fo = FetchOptions::new();
  fo.remote_callbacks(callbacks);
  fo.download_tags(AutotagOption::All);
  remote.fetch(&[] as &[&str], Some(&mut fo), None)
}

/// Retrieves the latest tag from the repository by parsing the tag names as versions
/// and returning the highest version.
pub fn get_latest_tag(name: &str, constraint: Option<VersionReq>) -> Result<Version, Box<dyn Error>> {
  let tags = get_remote_tags(name)?;
  let mut highest_version: Option<Version> = None;

  for tag in tags {
    if let Ok(version) = Version::parse(&tag) {
      let is_within_constraint = match &constraint {
        Some(req) => req.matches(&version),
        None => true,
      };

      if is_within_constraint && highest_version.as_ref().map_or(true, |latest| version > *latest) {
        highest_version = Some(version);
      }
    }
  }

  highest_version.ok_or_else(|| "No matching tags found".into())
}
