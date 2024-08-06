use super::{
  parser::{ParseBook, TermParser},
  Book, Name,
};
use crate::{
  diagnostics::{Diagnostics, DiagnosticsConfig, FileSpan, TextLocation, TextSpan},
  imports::PackageLoader,
};
use std::path::Path;

// TODO: Refactor so that we don't mix the two syntaxes here.

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(
  path: &Path,
  package_loader: impl PackageLoader,
  diag: DiagnosticsConfig,
) -> Result<Book, Diagnostics> {
  match path.try_exists() {
    Ok(exists) => {
      if !exists {
        return Err(format!("The file '{}' was not found.", path.display()).into());
      }
      let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
      load_to_book(path, &code, package_loader, diag)
    }
    Err(e) => Err(e.to_string().into()),
  }
}

pub fn load_to_book(
  origin: &Path,
  code: &str,
  package_loader: impl PackageLoader,
  diag: DiagnosticsConfig,
) -> Result<Book, Diagnostics> {
  let builtins = ParseBook::builtins();
  let book = do_parse_book(code, origin, builtins)?;
  book.load_imports(package_loader, diag)
}

pub fn do_parse_book(code: &str, origin: &Path, mut book: ParseBook) -> Result<ParseBook, Diagnostics> {
  book.source = Name::new(origin.to_string_lossy());
  TermParser::new(code).parse_book(book, false).map_err(|err| {
    let mut diagnostics = Diagnostics::default();
    let span = byte_span_to_line(code, err.span);
    diagnostics.add_parsing_error(err, FileSpan { span, file: origin.to_string_lossy().into() });
    diagnostics
  })
}

pub fn do_parse_book_default(code: &str, origin: &Path) -> Result<Book, Diagnostics> {
  do_parse_book(code, origin, ParseBook::builtins())?.to_fun()
}

fn byte_span_to_line(code: &str, span: (usize, usize)) -> TextSpan {
  // Will loop for way too long otherwise
  assert!(span.0 <= span.1);

  let mut start_line = 0;
  let mut start_char = 0;
  let mut end_line = 0;
  let mut end_char = 0;

  let mut curr_idx = 0;
  while curr_idx <= span.0 {
    if code.as_bytes()[curr_idx] == b'\n' {
      start_line += 1;
      end_line += 1;
      start_char = 0;
    } else {
      start_char += 1;
    }
    curr_idx += 1;
  }

  while curr_idx <= span.1 {
    if code.as_bytes()[curr_idx] == b'\n' {
      end_line += 1;
      end_char = 0;
    } else {
      end_char += 1;
    }
    curr_idx += 1;
  }

  (TextLocation { line: start_line, char: start_char }, TextLocation { line: end_line, char: end_char })
}
