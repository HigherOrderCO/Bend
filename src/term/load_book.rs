use super::{
  parser::{parse_definition_book, parser::error_to_msg},
  Book, Origin,
};
use itertools::Itertools;
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<Book, String> {
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  do_parse_book(&code, path)
}

pub fn do_parse_book(code: &str, path: &Path) -> Result<Book, String> {
  match parse_definition_book(code, Book::builtins, Origin::User) {
    Ok(book) => Ok(book),
    Err(errs) => Err(errs.iter().map(|e| error_to_msg(e, code, path)).join("\n")),
  }
}
