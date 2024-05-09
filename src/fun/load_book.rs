use crate::{
  fun::{self, parser::TermParser},
  imp::parser::PyParser,
};
use std::{collections::HashSet, path::Path};

// TODO: Refactor so that we don't mix the two syntaxes here.

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> Result<fun::Book, String> {
  let builtins = fun::Book::builtins();
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  if let Some(("", code)) = code.split_once("#flavor core") {
    do_parse_core_book(code, path, builtins)
  } else if let Some(("", code)) = code.split_once("#flavor bend") {
    do_parse_bend_book(code, path, builtins)
  } else {
    do_parse_bend_book(&code, path, builtins)
  }
}

pub fn do_parse_core_book(code: &str, path: &Path, builtins: fun::Book) -> Result<fun::Book, String> {
  TermParser::new(code).parse_book(builtins, false).map_err(|e| format!("In {} :\n{}", path.display(), e))
}

pub fn do_parse_bend_book(code: &str, path: &Path, builtins: fun::Book) -> Result<fun::Book, String> {
  let mut builtin_names = builtins.defs.keys().cloned().collect::<HashSet<_>>();
  builtin_names.extend(builtins.adts.keys().cloned());
  for adt in builtins.adts.values() {
    builtin_names.extend(adt.ctrs.keys().cloned());
  }
  let mut book = PyParser::new(code)
    .parse_program(&builtin_names)
    .map_err(|e| format!("In {} :\n{}", path.display(), e))?;

  // TODO: This should go somewhere else.
  book.order_kwargs();
  let book = book.to_fun(builtins);

  Ok(book)
}
