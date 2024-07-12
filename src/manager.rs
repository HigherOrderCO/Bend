use bend::diagnostics::Diagnostics;
use clap::Subcommand;
use git2::{FetchOptions, Repository};
use semver::Version;
use std::{
  error::Error,
  fs::{File, OpenOptions},
  io::Write,
  path::Path,
};
use toml_edit::{value, DocumentMut, Item, Table};

#[derive(Subcommand, Clone, Debug)]
pub enum PackageCmd {
  /// Initializes a bend module
  Init {
    #[arg(help = "Name of the module to initialize")]
    name: String,
  },
  /// Adds a dependency
  Get {
    #[arg(help = "Name of the dependency to load")]
    name: String,
    #[arg(help = "Version of the dependency")]
    version: Option<String>,
    #[arg(short = 'a', long, help = "Dependency alias")]
    alias: Option<String>,
  },
}

pub fn handle_package_cmd(command: PackageCmd) -> Result<(), Diagnostics> {
  match command {
    PackageCmd::Init { name } => init(&name).map_err(|e| e.to_string())?,
    PackageCmd::Get { name, version, alias} => get(&name, version, alias).map_err(|e| e.to_string())?,
  }

  Ok(())
}

fn init(name: &str) -> std::io::Result<()> {
  let mut config = File::create_new("mod.toml")?;
  config.write_all(format!("module = \"{name}\"").as_bytes())
}

fn get(name: &str, version: Option<String>, alias: Option<String>) -> Result<(), Box<dyn Error>> {
  let url = format!("https://{name}.git");

  let repo_name =
    alias.as_deref().unwrap_or_else(|| name.rsplit_once('/').expect("Invalid repository URL").1);
  let folder = format!(".bend/{}", repo_name);
  let local_path = Path::new(&folder);

  let tag = setup_repo(local_path, &url, version)?;

  update_mod(name, &tag, alias)
}

fn setup_repo(local_path: &Path, url: &str, version: Option<String>) -> Result<String, Box<dyn Error>> {
  // Check if the repository already exists
  let repo = match Repository::open(local_path) {
    Ok(repo) => repo,
    Err(_) => Repository::init(local_path)?,
  };

  {
    // Add the remote
    let remote_name = "origin";
    let mut remote = match repo.find_remote(remote_name) {
      Ok(remote) => {
        if remote.url() != Some(url) {
          repo.remote_set_url(remote_name, url)?;
          repo.find_remote(remote_name)?
        } else {
          remote
        }
      }
      Err(_) => repo.remote(remote_name, url)?,
    };

    delete_local_tags(&repo)?;

    // Fetch new tags from the remote
    let mut fetch_opts = FetchOptions::new();
    remote.fetch(&["refs/tags/*:refs/tags/*"], Some(&mut fetch_opts), None)?;
  }

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

fn delete_local_tags(repo: &Repository) -> Result<(), git2::Error> {
  let tags = repo.tag_names(None)?;
  for tag in tags.iter().flatten() {
    repo.tag_delete(tag)?;
  }
  Ok(())
}

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

fn update_mod(name: &str, version: &str, alias: Option<String>) -> Result<(), Box<dyn Error>> {
  let file = std::fs::read_to_string("mod.toml")?;
  let mut config = file.parse::<DocumentMut>().expect("invalid doc");
  let deps = config["dependencies"].or_insert(Item::Table(Table::new()));

  if let Some(table) = deps.as_table_like_mut() {
    // Check if the dependency already exists
    match table.get_mut(name) {
      Some(dep_item) => update_existing_dependency(dep_item, version, alias),
      None => _ = table.insert(name, new_dependency(version, alias)),
    }
  } else {
    panic!("Wrong `mod.toml` format, `dependencies` should be a table");
  }

  // Write the updated config back to mod.toml
  let mut f = OpenOptions::new().write(true).truncate(true).open("mod.toml")?;
  write!(f, "{}", config)?;

  Ok(())
}

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
