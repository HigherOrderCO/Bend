use crate::{
  diagnostics::{Diagnostics, DiagnosticsConfig, Severity, WarningType},
  fun::{Book, Name},
};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use std::fmt::Display;

pub mod book;
pub mod loader;
pub mod packages;

pub use loader::*;

pub type BindMap = IndexMap<Name, Name>;

#[derive(Debug, Clone, Default)]
pub struct ImportCtx {
  /// Imports declared in the program source.
  imports: Vec<Import>,

  /// Map from bound names to source package.
  map: ImportsMap,
}

impl ImportCtx {
  pub fn add_import(&mut self, import: Import) {
    self.imports.push(import);
  }

  pub fn to_imports(self) -> Vec<Import> {
    self.imports
  }

  pub fn sources(&self) -> Vec<&Name> {
    let mut names = Vec::new();
    for imps in &self.imports {
      match &imps.src {
        BoundSource::None => {}
        BoundSource::File(f) => names.push(f),
        BoundSource::Dir(v) => names.extend(v.values()),
        BoundSource::Either(_, _) => unreachable!("This should be resolved into `File` or `Dir` by now"),
      }
    }
    names
  }
}

#[derive(Debug, Clone)]
pub struct Import {
  pub path: Name,
  pub imp_type: ImportType,
  pub relative: bool,
  pub src: BoundSource,
}

impl Import {
  pub fn new(path: Name, imp_type: ImportType, relative: bool) -> Self {
    Self { path, imp_type, relative, src: BoundSource::None }
  }
}

impl Display for Import {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "from {} import {}", self.path, self.imp_type)
  }
}

#[derive(Debug, Clone)]
pub enum ImportType {
  Single(Name, Option<Name>),
  List(Vec<(Name, Option<Name>)>),
  Glob,
}

impl Display for ImportType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let format_alias = |name: &Name, alias: &Option<Name>| match alias {
      Some(alias) => format!("{name} as {alias}"),
      None => name.to_string(),
    };

    match self {
      ImportType::Single(n, a) => write!(f, "{}", format_alias(n, a)),
      ImportType::List(l) => write!(f, "({})", l.iter().map(|(n, a)| format_alias(n, a)).join(", ")),
      ImportType::Glob => write!(f, "*"),
    }
  }
}

#[derive(Debug, Clone)]
pub enum BoundSource {
  None,
  File(Name),
  Dir(IndexMap<Name, Name>),
  /// If the bound source is ambiguous between a file or a directory
  Either(Name, IndexMap<Name, Name>),
}

#[derive(Debug, Clone, Default)]
struct ImportsMap {
  binds: BindMap,
}

impl ImportsMap {
  pub fn contains_source(&self, s: &Name) -> bool {
    self.binds.values().contains(s)
  }

  fn add_bind(&mut self, src: &str, bind: Name, diag: &mut Diagnostics) {
    if let Some(old) = self.binds.get(&bind) {
      let warn = format!("The import '{src}' shadows the imported name '{old}'");
      diag.add_book_warning(warn, WarningType::ImportShadow);
    }

    self.binds.insert(bind, Name::new(src));
  }

  fn add_aliased_bind(&mut self, src: &Name, sub: &Name, alias: Option<&Name>, diag: &mut Diagnostics) {
    let src = format!("{}/{}", src, sub);
    let aliased = alias.unwrap_or(sub);
    self.add_bind(&src, aliased.clone(), diag);
  }

  fn add_binds(&mut self, names: &IndexSet<Name>, src: &Name, diag: &mut Diagnostics) {
    for sub in names {
      self.add_aliased_bind(src, sub, None, diag);
    }
  }

  /// Adds all names to the ImportMap in the form `alias/name`.
  /// If one of the names is equal to the file name, adds as `alias` instead.
  fn add_file_nested_binds(
    &mut self,
    src: &Name,
    file: &Name,
    alias: Option<&Name>,
    names: IndexSet<Name>,
    diag: &mut Diagnostics,
  ) {
    let aliased = alias.unwrap_or(file);

    self.add_nested_binds(src, aliased, names.iter().filter(|&n| n != file), diag);

    if names.contains(file) {
      let src = format!("{}/{}", src, file);
      self.add_bind(&src, aliased.clone(), diag);
    }
  }

  /// Adds all names to the ImportMap in the form `bind/name`.
  fn add_nested_binds<'a>(
    &mut self,
    src: &Name,
    bind: &Name,
    names: impl Iterator<Item = &'a Name>,
    diag: &mut Diagnostics,
  ) {
    for name in names {
      let src = format!("{}/{}", src, name);
      let bind = Name::new(format!("{bind}/{name}"));
      self.add_bind(&src, bind, diag);
    }
  }
}

#[allow(clippy::field_reassign_with_default)]
/// Check book without warnings about unused definitions
pub fn check_book(book: &mut Book) -> Result<Diagnostics, Diagnostics> {
  let mut diagnostics_cfg = DiagnosticsConfig::default();
  diagnostics_cfg.unused_definition = Severity::Allow;
  diagnostics_cfg.missing_main = Severity::Allow;

  let compile_opts = crate::CompileOpts::default();

  crate::check_book(book, diagnostics_cfg, compile_opts)
}
