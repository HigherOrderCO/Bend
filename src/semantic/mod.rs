/// Semantic checking
use crate::ast::{DefinitionBook, Name};

pub mod combinators;
/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.
pub mod pattern;
pub mod vars;

pub fn check_main(book: &DefinitionBook) -> anyhow::Result<()> {
  if !book.def_names.contains_name(&Name::from("Main".to_string())) {
    Err(anyhow::anyhow!("File has no 'Main' definition"))
  } else {
    Ok(())
  }
}
