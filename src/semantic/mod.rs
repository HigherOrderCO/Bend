/// Semantic checking
use crate::ast::{DefinitionBook, Name};

pub mod combinators;
pub mod flatten;
pub mod pattern;
pub mod vars;

pub fn check_main(book: &DefinitionBook) -> anyhow::Result<()> {
  if !book.def_names.contains_name(&Name::from("Main".to_string())) {
    Err(anyhow::anyhow!("File has no 'Main' definition"))
  } else {
    Ok(())
  }
}
