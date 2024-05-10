use crate::fun::{self, parser::TermParser};
use std::path::Path;

// TODO: Refactor so that we don't mix the two syntaxes here.

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<fun::Book, String> {
  let builtins = fun::Book::builtins();
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  do_parse_book(&code, path, builtins)
}

pub fn do_parse_book(code: &str, path: &Path, builtins: fun::Book) -> Result<fun::Book, String> {
  TermParser::new(code).parse_book(builtins, false).map_err(|e| format!("In {} :\n{}", path.display(), e))
}
