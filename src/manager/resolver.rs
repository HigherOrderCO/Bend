use super::config::{get_deps, get_version, CONFIG_FILE};
use super::repo;
use super::get_config;
use semver::{Version, VersionReq};
use std::collections::{BTreeMap, HashMap};
use std::error::Error;
use toml_edit::TableLike;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageInfo {
  pub name: String,
  pub version: Version,
}

#[derive(Debug, Clone)]
struct Package {
  info: PackageInfo,
  dependencies: Vec<PackageInfo>,
}

#[derive(Debug)]
struct DependencyGraph {
  packages: BTreeMap<String, Vec<Package>>,
}

pub type Resolved = BTreeMap<String, Vec<Version>>;
pub type Dependencies = HashMap<PackageInfo, Vec<PackageInfo>>;

pub fn resolve() -> Result<(Resolved, Dependencies), Box<dyn Error>> {
  let mut config = get_config(CONFIG_FILE)?;
  let deps = get_deps(&mut config)?;

  let graph = build_dependency_graph(deps)?;
  resolve_dependencies(&graph)
}

/// Builds the dependency graph from the given dependencies
fn build_dependency_graph(deps: &dyn TableLike) -> Result<DependencyGraph, Box<dyn Error>> {
  let mut graph = DependencyGraph { packages: BTreeMap::new() };

  for (name, dep) in deps.iter() {
    let version = get_version(dep).ok_or_else(|| format!("invalid version format '{}'", dep))?;
    let version = fun_name(version);

    populate_package_versions(&mut graph, name, &version)?;
  }

  Ok(graph)
}

fn populate_package_versions(
  graph: &mut DependencyGraph,
  name: &str,
  version_req: &Version,
) -> Result<(), Box<dyn Error>> {
  let versions = get_all_versions(name)?;

  println!("Available versions for {}[{}]: {:?}", name, version_req, versions);

  for version in versions {
    if version_req == &version {
      let dependencies = get_dependencies(name, &version)?;
      let info = PackageInfo { name: name.to_owned(), version: version.clone() };
      let package = Package { info, dependencies: dependencies.clone() };

      println!("Matching version for {}: {} with dependencies: {:?}", name, version, dependencies);

      // Recursively populate dependencies
      for dep in &package.dependencies {
        populate_package_versions(graph, &dep.name, &dep.version)?;
      }

      graph.packages.entry(name.to_string()).or_default().push(package);
      break;
    }
  }

  Ok(())
}

fn resolve_dependencies(graph: &DependencyGraph) -> Result<(Resolved, Dependencies), Box<dyn Error>> {
  let mut resolved = Resolved::new();
  let mut resolved_deps = Dependencies::new();

  for (name, pkgs) in &graph.packages {
    for pkg in pkgs {
      let pkg = PackageInfo { name: name.clone(), version: pkg.info.version.clone() };
      resolve_package(pkg, graph, &mut resolved, &mut resolved_deps)?;
    }
  }

  Ok((resolved, resolved_deps))
}

// TODO: MVS
fn resolve_package(
  pkg: PackageInfo,
  graph: &DependencyGraph,
  resolved: &mut Resolved,
  resolved_deps: &mut Dependencies,
) -> Result<(), Box<dyn Error>> {
  println!("resolve_package for {}[{}]", pkg.name, pkg.version);

  let versions = graph.packages.get(&pkg.name).ok_or_else(|| format!("package '{}' not found", pkg.name))?;
  let selected = versions.iter().find(|package| pkg.version == package.info.version).unwrap().clone();

  resolved.entry(pkg.name.to_owned()).or_default().push(selected.info.version.clone());
  resolved_deps.insert(pkg, selected.dependencies);

  Ok(())
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
fn get_dependencies(name: &str, version: &Version) -> Result<Vec<PackageInfo>, Box<dyn Error>> {
  let local_path = repo::clone(name, &version.to_string())?;

  let Ok(mut config) = get_config(&local_path.join(CONFIG_FILE).to_string_lossy()) else {
    // TODO: if config is not present, it's not a bend project, should return an error
    // For now, we're returning an empty Vec, which might lead to silent failures
    return Ok(Vec::new());
  };

  let mut deps = Vec::new();

  for (name, item) in get_deps(&mut config)?.iter() {
    let version = get_version(item).ok_or_else(|| format!("invalid version format '{}'", item))?;
    let version = fun_name(version);

    deps.push(PackageInfo { name: name.to_owned(), version });
  }

  Ok(deps)
}

// TEMPORARY: Remove when mod.toml only has the fully resolved version
fn fun_name(ver: &str) -> Version {
  let a = VersionReq::parse(ver).unwrap().comparators.remove(0);
  Version::new(a.major, a.minor.unwrap_or_default(), a.patch.unwrap_or_default())
}
