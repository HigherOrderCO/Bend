use TSPL::ParseError;

use crate::fun::{display::DisplayFn, Name};
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter},
};

pub const ERR_INDENT_SIZE: usize = 2;

#[derive(Debug, Clone, Default)]
pub struct Diagnostics {
  err_counter: usize,
  pub diagnostics: BTreeMap<DiagnosticOrigin, Vec<Diagnostic>>,
  pub config: DiagnosticsConfig,
}

#[derive(Debug, Clone, Copy)]
pub struct DiagnosticsConfig {
  pub verbose: bool,
  pub irrefutable_match: Severity,
  pub redundant_match: Severity,
  pub unreachable_match: Severity,
  pub unused_definition: Severity,
  pub repeated_bind: Severity,
  pub recursion_cycle: Severity,
  pub missing_main: Severity,
  pub import_shadow: Severity,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
  pub message: String,
  pub severity: Severity,
  pub span: Option<FileSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticOrigin {
  /// An error when parsing source code.
  Parsing,
  /// An error from the relationship between multiple top-level definitions.
  Book,
  /// An error in a pattern-matching function definition rule.
  Rule(Name),
  /// An error in a compiled inet.
  Inet(String),
  /// An error during readback of hvm-core run results.
  Readback,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
  Allow,
  Warning,
  Error,
}

#[derive(Debug, Clone, Copy)]
pub enum WarningType {
  IrrefutableMatch,
  RedundantMatch,
  UnreachableMatch,
  UnusedDefinition,
  RepeatedBind,
  RecursionCycle,
  MissingMain,
  ImportShadow,
}

impl Diagnostics {
  pub fn new(config: DiagnosticsConfig) -> Self {
    Self { err_counter: 0, diagnostics: Default::default(), config }
  }

  pub fn add_parsing_error(&mut self, err: impl std::fmt::Display, span: FileSpan) {
    self.err_counter += 1;
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Parsing, Some(span));
  }

  pub fn add_book_error(&mut self, err: impl std::fmt::Display) {
    self.err_counter += 1;
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Book, None);
  }

  pub fn add_rule_error(&mut self, err: impl std::fmt::Display, def_name: Name) {
    self.err_counter += 1;
    self.add_diagnostic(
      err,
      Severity::Error,
      DiagnosticOrigin::Rule(def_name.def_name_from_generated()),
      None,
    );
  }

  pub fn add_inet_error(&mut self, err: impl std::fmt::Display, def_name: String) {
    self.err_counter += 1;
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Inet(def_name), None);
  }

  pub fn add_rule_warning(
    &mut self,
    warn: impl std::fmt::Display,
    warn_type: WarningType,
    def_name: Name,
    span: Option<FileSpan>,
  ) {
    let severity = self.config.warning_severity(warn_type);
    if severity == Severity::Error {
      self.err_counter += 1;
    }
    self.add_diagnostic(warn, severity, DiagnosticOrigin::Rule(def_name.def_name_from_generated()), span);
  }

  pub fn add_book_warning(&mut self, warn: impl std::fmt::Display, warn_type: WarningType) {
    let severity = self.config.warning_severity(warn_type);
    if severity == Severity::Error {
      self.err_counter += 1;
    }
    self.add_diagnostic(warn, severity, DiagnosticOrigin::Book, None);
  }

  pub fn add_diagnostic(
    &mut self,
    msg: impl ToString,
    severity: Severity,
    orig: DiagnosticOrigin,
    range: Option<FileSpan>,
  ) {
    let diag = Diagnostic { message: msg.to_string(), severity, span: range };
    self.diagnostics.entry(orig).or_default().push(diag)
  }

  pub fn take_rule_err<T, E: std::fmt::Display>(
    &mut self,
    result: Result<T, E>,
    def_name: Name,
  ) -> Option<T> {
    match result {
      Ok(t) => Some(t),
      Err(e) => {
        self.add_rule_error(e, def_name);
        None
      }
    }
  }

  pub fn take_inet_err<T, E: std::fmt::Display>(
    &mut self,
    result: Result<T, E>,
    def_name: String,
  ) -> Option<T> {
    match result {
      Ok(t) => Some(t),
      Err(e) => {
        self.add_inet_error(e, def_name);
        None
      }
    }
  }

  pub fn has_severity(&self, severity: Severity) -> bool {
    self.diagnostics.values().any(|errs| errs.iter().any(|e| e.severity == severity))
  }

  pub fn has_errors(&self) -> bool {
    self.has_severity(Severity::Error)
  }

  /// Resets the internal counter
  pub fn start_pass(&mut self) {
    self.err_counter = 0;
  }

  /// Checks if any error was emitted since the start of the pass,
  /// Returning all the current information as a `Err(Info)`, replacing `&mut self` with an empty one.
  /// Otherwise, returns the given arg as an `Ok(T)`.
  pub fn fatal<T>(&mut self, t: T) -> Result<T, Diagnostics> {
    if self.err_counter == 0 {
      Ok(t)
    } else {
      Err(std::mem::take(self))
    }
  }

  /// Returns a Display that prints the diagnostics with one of the given severities.
  pub fn display_with_severity(&self, severity: Severity) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| {
      fn filter<'a>(
        errs: impl IntoIterator<Item = &'a Diagnostic>,
        severity: Severity,
      ) -> impl Iterator<Item = &'a Diagnostic> {
        errs.into_iter().filter(move |err| err.severity == severity)
      }

      let mut has_msg = false;
      for (orig, errs) in &self.diagnostics {
        let mut errs = filter(errs, severity).peekable();
        if errs.peek().is_some() {
          match orig {
            DiagnosticOrigin::Parsing => {
              for err in errs {
                writeln!(f, "{err}")?;
              }
            }
            DiagnosticOrigin::Book => {
              for err in errs {
                writeln!(f, "{err}")?;
              }
            }
            DiagnosticOrigin::Rule(nam) => {
              writeln!(f, "\x1b[1mIn definition '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
              for err in errs {
                writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
              }
            }
            DiagnosticOrigin::Inet(nam) => {
              writeln!(f, "\x1b[1mIn compiled inet '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
              for err in errs {
                writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
              }
            }
            DiagnosticOrigin::Readback => {
              writeln!(f, "\x1b[1mDuring readback:\x1b[0m")?;
              for err in errs {
                writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
              }
            }
          }
          has_msg = true;
        }
      }
      if has_msg {
        writeln!(f)?;
      }
      Ok(())
    })
  }

  //   pub fn display_with_severity<'a>(
  //     &'a self,
  //     severity: Severity,
  //     origin: &'a DiagnosticOrigin,
  //   ) -> impl std::fmt::Display + '_ {
  //     DisplayFn(move |f| {
  //       fn filter<'a>(
  //         errs: impl IntoIterator<Item = &'a Diagnostic>,
  //         severity: Severity,
  //       ) -> impl Iterator<Item = &'a Diagnostic> {
  //         errs.into_iter().filter(move |err| err.severity == severity)
  //       }

  //       let mut has_msg = false;
  //       if let Some(errs) = self.diagnostics.get(&origin) {
  //         let mut errs = filter(errs, severity).peekable();
  //         if errs.peek().is_some() {
  //           match &origin {
  //             DiagnosticOrigin::Parsing => {
  //               for err in errs {
  //                 writeln!(f, "{err}")?;
  //               }
  //             }
  //             DiagnosticOrigin::Book => {
  //               for err in errs {
  //                 writeln!(f, "{err}")?;
  //               }
  //             }
  //             DiagnosticOrigin::Rule(nam) => {
  //               writeln!(f, "\x1b[1mIn definition '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
  //               for err in errs {
  //                 writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
  //               }
  //             }
  //             DiagnosticOrigin::Inet(nam) => {
  //               writeln!(f, "\x1b[1mIn compiled inet '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
  //               for err in errs {
  //                 writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
  //               }
  //             }
  //             DiagnosticOrigin::Readback => {
  //               writeln!(f, "\x1b[1mDuring readback:\x1b[0m")?;
  //               for err in errs {
  //                 writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
  //               }
  //             }
  //           }
  //           has_msg = true;
  //         }
  //       }
  //       if has_msg {
  //         writeln!(f)?;
  //       }
  //       Ok(())
  //     })
  //   }

  //   /// Returns a Display that prints the diagnostics with one of the given severities.
  //   pub fn display_all_with_severity(&self, severity: Severity) -> impl std::fmt::Display + '_ {
  //     DisplayFn(move |f| {
  //       let mut has_msg = false;
  //       for orig in self.diagnostics.keys() {
  //         write!(f, "{}", self.display_with_severity(severity, orig))?;
  //       }
  //       if has_msg {
  //         writeln!(f)?;
  //       }
  //       Ok(())
  //     })
  //   }
}

impl Display for Diagnostics {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if self.has_severity(Severity::Warning) {
      write!(f, "\x1b[4m\x1b[1m\x1b[33mWarnings:\x1b[0m\n{}", self.display_with_severity(Severity::Warning))?;
    }
    if self.has_severity(Severity::Error) {
      write!(f, "\x1b[4m\x1b[1m\x1b[31mErrors:\x1b[0m\n{}", self.display_with_severity(Severity::Error))?;
    }
    Ok(())
  }
}

impl From<String> for Diagnostics {
  fn from(value: String) -> Self {
    Self {
      diagnostics: BTreeMap::from_iter([(
        DiagnosticOrigin::Book,
        vec![Diagnostic { message: value, severity: Severity::Error, span: None }],
      )]),
      ..Default::default()
    }
  }
}

impl From<ParseError> for Diagnostics {
  fn from(value: ParseError) -> Self {
    Self {
      diagnostics: BTreeMap::from_iter([(
        DiagnosticOrigin::Parsing,
        // TODO: range is not because we're missing the origin file, can we fix this?
        vec![Diagnostic { message: value.into(), severity: Severity::Error, span: None }],
      )]),
      ..Default::default()
    }
  }
}

impl DiagnosticsConfig {
  pub fn new(severity: Severity, verbose: bool) -> Self {
    Self {
      irrefutable_match: severity,
      redundant_match: severity,
      unreachable_match: severity,
      unused_definition: severity,
      repeated_bind: severity,
      recursion_cycle: severity,
      import_shadow: severity,
      // Should only be changed manually, as a missing main is always a error to hvm
      missing_main: Severity::Error,
      verbose,
    }
  }

  pub fn warning_severity(&self, warn: WarningType) -> Severity {
    match warn {
      WarningType::UnusedDefinition => self.unused_definition,
      WarningType::RepeatedBind => self.repeated_bind,
      WarningType::RecursionCycle => self.recursion_cycle,
      WarningType::IrrefutableMatch => self.irrefutable_match,
      WarningType::RedundantMatch => self.redundant_match,
      WarningType::UnreachableMatch => self.unreachable_match,
      WarningType::MissingMain => self.missing_main,
      WarningType::ImportShadow => self.import_shadow,
    }
  }
}

impl Default for DiagnosticsConfig {
  fn default() -> Self {
    let mut cfg = Self::new(Severity::Warning, false);
    cfg.recursion_cycle = Severity::Error;
    cfg
  }
}

impl Display for Diagnostic {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.span {
      Some(FileSpan { file: Some(file), .. }) => write!(f, "In {} :\n{}", file, self.message),
      _ => write!(f, "{}", self.message),
    }
  }
}

impl Diagnostic {
  pub fn display_with_origin<'a>(&'a self, origin: &'a DiagnosticOrigin) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| {
      match origin {
        DiagnosticOrigin::Parsing => writeln!(f, "{self}")?,
        DiagnosticOrigin::Book => writeln!(f, "{self}")?,
        DiagnosticOrigin::Rule(nam) => {
          writeln!(f, "\x1b[1mIn definition '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
          writeln!(f, "{:ERR_INDENT_SIZE$}{self}", "")?;
        }
        DiagnosticOrigin::Inet(nam) => {
          writeln!(f, "\x1b[1mIn compiled inet '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
          writeln!(f, "{:ERR_INDENT_SIZE$}{self}", "")?;
        }
        DiagnosticOrigin::Readback => {
          writeln!(f, "\x1b[1mDuring readback:\x1b[0m")?;
          writeln!(f, "{:ERR_INDENT_SIZE$}{self}", "")?;
        }
      };
      Ok(())
    })
  }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub struct TextLocation {
  pub line: usize,
  pub char: usize,
}

impl TextLocation {
  pub fn new(line: usize, char: usize) -> Self {
    TextLocation { line, char }
  }

  pub fn from_byte_loc(code: &str, loc: usize) -> Self {
    let code = code.as_bytes();
    let mut line = 0;
    let mut char = 0;
    let mut curr_idx = 0;
    while curr_idx < loc && curr_idx < code.len() {
      if code[curr_idx] == b'\n' {
        line += 1;
        char = 0;
      } else {
        char += 1;
      }
      curr_idx += 1;
    }

    TextLocation { line, char }
  }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub struct TextSpan {
  pub start: TextLocation,
  pub end: TextLocation,
}

impl TextSpan {
  pub fn new(start: TextLocation, end: TextLocation) -> Self {
    TextSpan { start, end }
  }

  pub fn from_byte_span(code: &str, span: (usize, usize)) -> Self {
    // Will loop for way too long otherwise
    assert!(span.0 <= span.1);

    let code = code.as_bytes();
    let mut start_line = 0;
    let mut start_char = 0;
    let mut end_line;
    let mut end_char;

    let mut curr_idx = 0;
    while curr_idx < span.0 && curr_idx < code.len() {
      if code[curr_idx] == b'\n' {
        start_line += 1;
        start_char = 0;
      } else {
        start_char += 1;
      }
      curr_idx += 1;
    }

    end_line = start_line;
    end_char = start_char;
    while curr_idx < span.1 && curr_idx < code.len() {
      if code[curr_idx] == b'\n' {
        end_line += 1;
        end_char = 0;
      } else {
        end_char += 1;
      }
      curr_idx += 1;
    }

    TextSpan::new(TextLocation::new(start_line, start_char), TextLocation::new(end_line, end_char))
  }
}

#[derive(Debug, Clone, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub struct FileSpan {
  pub span: TextSpan,
  // Storing files as Strings, could be done as file IDs in the future
  // This is currently optional, we might want to change it later
  pub file: Option<String>,
}

impl FileSpan {
  pub fn new(span: TextSpan, origin: Option<&str>) -> Self {
    FileSpan { span, file: origin.map(|s| s.into()) }
  }
}
