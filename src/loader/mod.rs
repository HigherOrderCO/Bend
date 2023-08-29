use crate::{ast::DefinitionBook, parser::parse_definition_book};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::path::Path;

/// Reads a file and parses to a definition book.
pub fn load_file_to_book(path: &Path) -> anyhow::Result<DefinitionBook> {
  let code = std::fs::read_to_string(path)?;
  match parse_definition_book(&code) {
    Ok(book) => Ok(book),
    Err(errs) => {
      for err in errs {
        Report::build(ReportKind::Error, (), err.span().start)
          .with_code(3)
          .with_message(err.to_string())
          .with_label(
            Label::new(err.span().into_range()).with_message(err.reason().to_string()).with_color(Color::Red),
          )
          .finish()
          .eprint(Source::from(code.clone()))
          .unwrap();
      }
      Err(anyhow::anyhow!("Parsing error"))
    }
  }
}
