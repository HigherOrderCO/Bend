use super::{parser::TermParser, Book};
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<Book, String> {
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  do_parse_book(&code, path)
}

pub fn do_parse_book(code: &str, path: &Path) -> Result<Book, String> {
  TermParser::new(code)
    .parse_book(Book::builtins(), false)
    .map_err(|e| format!("In {} :\n{}", path.display(), e))
}
