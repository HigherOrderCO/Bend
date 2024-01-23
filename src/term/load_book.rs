use crate::term::{
  parser::{parse_definition_book, parser::error_to_msg},
  Book,
};
use itertools::Itertools;
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<Book, String> {
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      let msg = errs.iter().map(|e| error_to_msg(e, &code)).join("\n");
      Err(msg)
    }
  }
}
