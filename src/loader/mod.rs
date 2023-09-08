use crate::{ast::DefinitionBook, parser::parse_definition_book};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::Rich;
use itertools::Itertools;
use std::{fmt::Display, ops::Range, path::Path};

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> anyhow::Result<DefinitionBook> {
  let code = std::fs::read_to_string(path)?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      let msg = errs.into_iter().map(|e| display_err_for_console(e, path, &code)).join("\n");
      Err(anyhow::anyhow!(msg))
    }
  }
}

pub fn display_err_for_console<T: Display>(err: Rich<T>, path: &Path, code: &str) -> String {
  let path = path.to_string_lossy();
  let report = err_to_report(err, &path);
  let mut out: Vec<u8> = vec![];
  report.write((path.as_ref(), Source::from(code)), &mut out).unwrap();
  String::from_utf8(out).unwrap()
}

pub fn display_err_for_text<T: Display>(err: Rich<T>) -> String {
  err.to_string()
}

pub fn err_to_report<'a, T: Display>(err: Rich<T>, path: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
  Report::build(ReportKind::Error, path, err.span().start)
    .with_message(err.to_string())
    .with_label(
      Label::new((path, err.span().into_range()))
        .with_message(err.reason().to_string())
        .with_color(Color::Red),
    )
    .finish()
}
