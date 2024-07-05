use bend::{
  diagnostics::{Diagnostics, DiagnosticsConfig},
  fun::load_book::load_to_book,
  imports::{check_book, DefaultLoader},
};
use bpm::*;
use clap::Subcommand;
use std::path::PathBuf;

#[derive(Subcommand, Clone, Debug)]
pub enum PackageCmd {
  /// Stores a bend file into the package manager
  Store {
    #[arg(help = "Path to the input file")]
    path: PathBuf,
    #[arg(
      short = 'n',
      long = "name",
      help = "Overwrite the name of the package, otherwise the file name is used"
    )]
    name: Option<String>,
    #[arg(help = "Namespace to store the file")]
    namespace: String,
    #[arg(help = "Version of the package", default_value = "v0")]
    version: String,
  },
  /// Loads a package from the package manager
  Load {
    #[arg(help = "Name of the package to load")]
    name: String,
  },
  /// Checks fo permissions to create/update the target package
  CanPost {
    #[arg(help = "Name of the package to check for permissions")]
    name: String,
  },
  /// Adds permissions to update the target package to the given user name
  AddPerms {
    #[arg(help = "Name of the package/namespace to modify the permissions")]
    name: String,
    #[arg(help = "Username to add the edit/create permissions to")]
    user: String,
  },
  /// Makes a namespace public, allowing all users to create packages inside
  MakePub {
    #[arg(help = "Name of the namespace to make public")]
    name: String,
  },
  /// Register a new Bend Package Manager account
  Register,
}

pub fn handle_package_cmd(command: PackageCmd) -> Result<(), Diagnostics> {
  match command {
    PackageCmd::CanPost { name } => match can_post(&PackageDescriptor::from(name.as_str())) {
      Ok(true) => println!("The package `{name}` can be uploaded"),
      Ok(false) => println!("The package `{name}` can NOT be uploaded"),
      err @ Err(_) => _ = err?,
    },

    PackageCmd::AddPerms { name, user } => add_perms(PackageDescriptor::from(name.as_str()), User(user))?,

    PackageCmd::Store { path, name, namespace, version } => store_cmd(path, name, namespace, version)?,

    PackageCmd::Load { name } => load_cmd(&name).map(|pack| println!("{}", pack))?,

    PackageCmd::MakePub { name } => make_public(name)?,

    PackageCmd::Register => register_cmd()?,
  };

  Ok(())
}

fn store_cmd(
  path: PathBuf,
  name: Option<String>,
  namespace: String,
  version: String,
) -> Result<(), Diagnostics> {
  let name = name.unwrap_or_else(|| {
    let path = path.clone().with_extension("");
    let file_name = path.file_name().unwrap();
    file_name.to_string_lossy().to_string()
  });

  let package_name = format!("{}/{}", namespace, name);

  let pack = PackageDescriptor::new(Some(&version), &package_name);
  let package = check(path)?;
  store(pack, package)?;

  Ok(())
}

pub fn load_cmd(name: &str) -> Result<String, String> {
  load(&PackageDescriptor::from(name)).map(|Package(pack)| pack)
}

fn check(path: PathBuf) -> Result<Package, Diagnostics> {
  let source = std::fs::read_to_string(&path).map_err(|e| e.to_string())?;
  let package_loader = DefaultLoader::new_empty(load_cmd);
  let mut book = load_to_book(&path, &source, package_loader, DiagnosticsConfig::default())?;

  let diagnostics = check_book(&mut book)?;
  eprint!("{diagnostics}");

  Ok(Package(source))
}

fn register_cmd() -> Result<(), String> {
  use std::io::{stdout, Write};

  print!("Please enter your username: ");
  let _ = stdout().flush();

  let user = get_input();

  loop {
    print!("Please enter your password:  ");
    let _ = stdout().flush();
    let pass = rpassword::read_password().unwrap();

    print!("Please repeat your password: ");
    let _ = stdout().flush();
    let pass2 = rpassword::read_password().unwrap();

    if pass != pass2 {
      println!("Passwords do not match, please try again")
    } else {
      register_user(&user, &pass)?;
      println!("User `{user}` registered");
      return Ok(());
    }
  }
}

fn get_input() -> String {
  use std::io::stdin;

  let mut user = String::new();
  stdin().read_line(&mut user).expect("Did not enter a correct string");

  if let Some('\n') = user.chars().next_back() {
    user.pop();
  }

  if let Some('\r') = user.chars().next_back() {
    user.pop();
  }

  user
}
