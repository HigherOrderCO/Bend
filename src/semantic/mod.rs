/// Semantic checking
use crate::ast::{DefId, DefinitionBook, Name};

/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.
pub mod pattern;
pub mod vars;

pub fn check_main(book: &DefinitionBook) -> anyhow::Result<DefId> {
  if let Some(main) = book.def_names.def_id(&Name::new("Main")) {
    Ok(main)
  } else {
    Err(anyhow::anyhow!("File has no 'Main' definition"))
  }
}
