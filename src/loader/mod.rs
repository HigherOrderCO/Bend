use crate::{ast::DefinitionBook, parser::parse_definition_book};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::Rich;
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> anyhow::Result<DefinitionBook> {
  let code = std::fs::read_to_string(path)?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      print_err_reports(&path.to_string_lossy(), &code, errs);
      Err(anyhow::anyhow!("Parsing error"))
    }
  }
}

pub fn print_err_reports<T: std::fmt::Display>(path: &str, code: &str, errs: Vec<Rich<T>>) {
  errs.into_iter().for_each(|err| {
    Report::build(ReportKind::Error, path, err.span().start)
      .with_message(err.to_string())
      .with_label(
        Label::new((path, err.span().into_range()))
          .with_message(err.reason().to_string())
          .with_color(Color::Red),
      )
      .finish()
      .eprint((path, Source::from(code.to_string())))
      .unwrap()
  })
}
