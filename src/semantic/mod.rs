/// Semantic checking
use crate::ast::{DefId, DefinitionBook};

pub mod affine;

pub fn check_main(book: &DefinitionBook) -> anyhow::Result<()> {
  if !book.defs.contains_key(&DefId::from("Main")) {
    Err(anyhow::anyhow!("File has no 'Main' definition"))
  } else {
    Ok(())
  }
}
