use super::parser::parse_definition_book;
use crate::ast::DefinitionBook;
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::Rich;
use itertools::Itertools;
use miette::{diagnostic, miette, Diagnostic, NamedSource, SourceSpan};
use std::{fmt::Display, ops::Range, path::Path};

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> anyhow::Result<DefinitionBook> {
  let code = std::fs::read_to_string(path)?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      let msg = errs.into_iter().map(|e| display_miette_err(e, path, &code)).join("\n");
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

/// Displays a formatted [SyntaxError] from the given `err` based on the current report handler.
pub fn display_miette_err<T: Display>(err: Rich<T>, path: &Path, code: &str) -> String {
  let source = code.to_string();
  let name = path.to_str().unwrap();

  let src = NamedSource::new(name, source);
  let error = SyntaxError::from_rich(err, src);

  let report = miette!(error);

  format!("{report:?}")
}

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("{}", error)]
#[diagnostic()]
/// This structure holds information for syntax errors.
struct SyntaxError {
  /// The error name.
  error: String,
  #[source_code]
  /// The file name and source.
  src: NamedSource,
  #[label("{}", reason)]
  /// The error byte range.
  span: SourceSpan,
  /// The error reason.
  reason: String,
}

impl SyntaxError {
  /// Creates a new [SyntaxError] from a [Rich<T>] `err`.
  fn from_rich<T: Display>(err: Rich<T>, src: NamedSource) -> Self {
    let error = err.to_string();
    let reason = err.reason().to_string();
    let span = SourceSpan::from(err.span().into_range());

    Self { error, reason, span, src }
  }
}
