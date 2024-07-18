use super::config::{get_deps, get_version, CONFIG_FILE};
use super::repo;
use super::{get_config, lock::update_lock_file};
use semver::{Version, VersionReq};
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use toml_edit::TableLike;

struct Dependency {
  name: String,
  version_req: VersionReq,
}

struct Package {
  #[allow(dead_code)]
  name: String,
  version: Version,
  dependencies: Vec<Dependency>,
}

struct DependencyGraph {
  packages: BTreeMap<String, Vec<Package>>,
}

pub fn resolve() -> Result<(), Box<dyn Error>> {
  let mut config = get_config(CONFIG_FILE)?;
  let deps = get_deps(&mut config)?;

  let graph = build_dependency_graph(deps)?;
  let resolved = resolve_dependencies(&graph)?;

  update_lock_file(resolved)?;

  Ok(())
}

/// Builds the dependency graph from the given dependencies
fn build_dependency_graph(deps: &dyn TableLike) -> Result<DependencyGraph, Box<dyn Error>> {
  let mut graph = DependencyGraph { packages: BTreeMap::new() };

  for (name, dep) in deps.iter() {
    let ver = get_version(dep).ok_or_else(|| format!("Invalid version format '{}'", dep))?;
    let version_req = VersionReq::parse(ver)?;

    let versions = get_all_versions(name)?;

    // Find the latest version that matches the version requirement
    let latest_version =
      versions.iter().filter(|version| version_req.matches(version)).max().ok_or_else(|| {
        format!("No matching versions found for '{}' with requirement {}", name, version_req)
      })?;

    let package = Package {
      name: name.to_string(),
      version: latest_version.clone(),
      dependencies: get_dependencies(name, latest_version)?,
    };

    graph.packages.entry(name.to_string()).or_default().push(package);
  }

  Ok(graph)
}

/// Resolves dependencies using Minimal Version Selection
fn resolve_dependencies(graph: &DependencyGraph) -> Result<BTreeMap<String, Version>, Box<dyn Error>> {
  let mut resolved = BTreeMap::new();
  let mut visited = BTreeSet::new();

  for name in graph.packages.keys() {
    if !visited.contains(name) {
      resolve_package(name, graph, &mut resolved, &mut visited)?;
    }
  }

  Ok(resolved)
}

/// Recursively resolves a single package and its dependencies using MVS
fn resolve_package(
  name: &str,
  graph: &DependencyGraph,
  resolved: &mut BTreeMap<String, Version>,
  visited: &mut BTreeSet<String>,
) -> Result<(), Box<dyn Error>> {
  if visited.contains(name) {
    return Ok(());
  }

  visited.insert(name.to_string());

  let versions = graph.packages.get(name).ok_or_else(|| format!("Package '{}' not found", name))?;

  // MVS: Select the minimum version that satisfies all constraints
  let mut selected_version = None;
  for package in versions {
    if selected_version.as_ref().map_or(true, |v| package.version < *v) {
      let is_compatible = package
        .dependencies
        .iter()
        .all(|dep| resolved.get(&dep.name).map_or(true, |v| dep.version_req.matches(v)));

      if is_compatible {
        selected_version = Some(package.version.clone());
      }
    }
  }

  match selected_version {
    Some(version) => {
      resolved.insert(name.to_string(), version.clone());

      // Resolve dependencies of the selected version
      let selected_package = versions.iter().find(|p| p.version == version).unwrap();
      for dep in &selected_package.dependencies {
        resolve_package(&dep.name, graph, resolved, visited)?;
      }
      Ok(())
    }
    None => Err(format!("Unable to find a compatible version for '{}'", name).into()),
  }
}

/// Retrieves all versions for a given package
fn get_all_versions(name: &str) -> Result<Vec<Version>, Box<dyn Error>> {
  let tags = repo::get_remote_tags(name)?;
  let mut versions = Vec::new();

  for tag in tags {
    if let Ok(version) = Version::parse(&tag) {
      versions.push(version);
    }
  }

  versions.sort();
  Ok(versions)
}

/// Retrieves dependencies for a specific version of a package
fn get_dependencies(name: &str, version: &Version) -> Result<Vec<Dependency>, Box<dyn Error>> {
  let local_path = repo::clone(name, &version.to_string(), None)?;

  let Ok(mut config) = get_config(&local_path.join(CONFIG_FILE).to_string_lossy()) else {
    // TODO: if config is not present, it's not a bend project, should return an error
    // For now, we're returning an empty Vec, which might lead to silent failures
    return Ok(Vec::new());
  };

  let mut deps = Vec::new();

  for (name, item) in get_deps(&mut config)?.iter() {
    let ver = get_version(item).ok_or_else(|| format!("Invalid version format '{}'", item))?;
    let version_req = VersionReq::parse(ver)?;

    deps.push(Dependency { name: name.to_string(), version_req });
  }

  Ok(deps)
}
