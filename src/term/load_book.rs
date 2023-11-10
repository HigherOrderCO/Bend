use super::{parser::parse_definition_book, Book};
use itertools::Itertools;
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<Book, String> {
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      let msg = errs.into_iter().map(|e| e.to_string()).join("\n");
      Err(msg)
    }
  }
}
