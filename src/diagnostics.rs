use TSPL::ParseError;

use crate::fun::{display::DisplayFn, Name, Source};
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter},
  ops::Range,
};

pub const ERR_INDENT_SIZE: usize = 2;

#[derive(Debug, Clone, Default)]
pub struct Diagnostics {
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
  pub source: Source,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticOrigin {
  /// An error when parsing source code.
  Parsing,
  /// An error from the relationship between multiple top-level definitions.
  Book,
  /// An error in a function definition.
  Function(Name),
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
    Self { diagnostics: Default::default(), config }
  }

  pub fn add_parsing_error(&mut self, err: impl std::fmt::Display, source: Source) {
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Parsing, source);
  }

  pub fn add_book_error(&mut self, err: impl std::fmt::Display) {
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Book, Default::default());
  }

  pub fn add_function_error(&mut self, err: impl std::fmt::Display, name: Name, source: Source) {
    self.add_diagnostic(
      err,
      Severity::Error,
      DiagnosticOrigin::Function(name.def_name_from_generated()),
      source,
    );
  }

  pub fn add_inet_error(&mut self, err: impl std::fmt::Display, def_name: String) {
    self.add_diagnostic(err, Severity::Error, DiagnosticOrigin::Inet(def_name), Default::default());
  }

  pub fn add_function_warning(
    &mut self,
    warn: impl std::fmt::Display,
    warn_type: WarningType,
    def_name: Name,
    source: Source,
  ) {
    let severity = self.config.warning_severity(warn_type);
    self.add_diagnostic(
      warn,
      severity,
      DiagnosticOrigin::Function(def_name.def_name_from_generated()),
      source,
    );
  }

  pub fn add_book_warning(&mut self, warn: impl std::fmt::Display, warn_type: WarningType) {
    let severity = self.config.warning_severity(warn_type);
    self.add_diagnostic(warn, severity, DiagnosticOrigin::Book, Default::default());
  }

  pub fn add_diagnostic(
    &mut self,
    msg: impl std::fmt::Display,
    severity: Severity,
    orig: DiagnosticOrigin,
    source: Source,
  ) {
    let diag = Diagnostic { message: msg.to_string(), severity, source };
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
        self.add_function_error(e, def_name, Default::default());
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

  /// Checks if any error was emitted since the start of the pass,
  /// Returning all the current information as a `Err(Info)`, replacing `&mut self` with an empty one.
  /// Otherwise, returns the given arg as an `Ok(T)`.
  pub fn fatal<T>(&mut self, t: T) -> Result<T, Diagnostics> {
    if !self.has_errors() {
      Ok(t)
    } else {
      Err(std::mem::take(self))
    }
  }

  /// Returns a Display that prints the diagnostics with one of the given severities.
  pub fn display_with_severity(&self, severity: Severity) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| {
      // We want to print diagnostics information somewhat like this:
      // ```
      // In file A :
      // In definition X :
      //   {error}
      // In definition Y :
      //   {error}
      //
      // In file B :
      // In compiled Inet Z :
      //   {error}
      //
      // Other diagnostics:
      // In {...}
      // ```
      // The problem is, diagnostics data is currently structured as a mapping from something like
      // DiagnosticOrigin to Vec<(DiagnosticMessage, DiagnosticFile)>, and we would need something
      // like a mapping from DiagnosticFile to DiagnosticOrigin to Vec<DiagnosticMessage> in order
      // to print it cleanly. We might want to change it later to have this structure,
      // but meanwhile, we do the transformations below to make the goal possible.

      // Ignore diagnostics without the desired severity.
      let diagnostics = self
        .diagnostics
        .iter()
        .map(|(origin, diags)| (origin, diags.iter().filter(|diag| diag.severity == severity)));

      // Produce the structure described above.
      let groups: BTreeMap<&Option<String>, BTreeMap<&DiagnosticOrigin, Vec<&Diagnostic>>> = diagnostics
        .fold(BTreeMap::new(), |mut file_tree, (origin, diags)| {
          for diag in diags {
            // We need to allow this Clippy warning due to `Name` in `DiagnosticOrigin::Function`.
            // We know how it works, so it shouldn't be a problem.
            #[allow(clippy::mutable_key_type)]
            let file_group_entry = file_tree.entry(&diag.source.file).or_default();
            let origin_group_entry = file_group_entry.entry(origin).or_default();
            origin_group_entry.push(diag);
          }
          file_tree
        });
      // Now, we have a mapping from DiagnosticFile to DiagnosticOrigin to Vec<DiagnosticMessage>.

      // If the last file is `None`, it means we only have diagnostics with unknown source file.
      // In this case, we won't print a special message for them.
      let only_unknown_file_diagnostics = groups.keys().next_back() == Some(&&None);

      // Reverse the group iterator so `None` files go last.
      for (file, origin_to_diagnostics) in groups.iter().rev() {
        if !only_unknown_file_diagnostics {
          match &file {
            Some(name) => writeln!(f, "\x1b[1mIn \x1b[4m{}\x1b[0m\x1b[1m :\x1b[0m", name)?,
            None => writeln!(f, "\x1b[1mOther diagnostics:\x1b[0m")?,
          };
        }

        let mut has_msg = false;
        for (origin, diagnostics) in origin_to_diagnostics {
          let mut diagnostics = diagnostics.iter().peekable();
          if diagnostics.peek().is_some() {
            match origin {
              DiagnosticOrigin::Parsing => {
                for err in diagnostics {
                  writeln!(f, "{err}")?;
                }
              }
              DiagnosticOrigin::Book => {
                for err in diagnostics {
                  writeln!(f, "{err}")?;
                }
              }
              DiagnosticOrigin::Function(nam) => {
                writeln!(f, "\x1b[1mIn definition '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
                for err in diagnostics {
                  writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
                }
              }
              DiagnosticOrigin::Inet(nam) => {
                writeln!(f, "\x1b[1mIn compiled inet '\x1b[4m{}\x1b[0m\x1b[1m':\x1b[0m", nam)?;
                for err in diagnostics {
                  writeln!(f, "{:ERR_INDENT_SIZE$}{err}", "")?;
                }
              }
              DiagnosticOrigin::Readback => {
                writeln!(f, "\x1b[1mDuring readback:\x1b[0m")?;
                for err in diagnostics {
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
      }
      Ok(())
    })
  }

  pub fn display_only_messages(&self) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| {
      for err in self.diagnostics.values().flatten() {
        writeln!(f, "{err}")?;
      }
      Ok(())
    })
  }
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
        vec![Diagnostic { message: value, severity: Severity::Error, source: Default::default() }],
      )]),
      ..Default::default()
    }
  }
}

impl From<ParseError> for Diagnostics {
  /// Transforms a parse error into `Diagnostics`.
  ///
  /// NOTE: Since `ParseError` does not include the source code, we can't get the `TextLocation` of the error,
  /// so it is not included in the diagnostic.
  /// range is set as None.
  fn from(value: ParseError) -> Self {
    Self {
      diagnostics: BTreeMap::from_iter([(
        DiagnosticOrigin::Parsing,
        vec![Diagnostic { message: value.into(), severity: Severity::Error, source: Default::default() }],
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
    write!(f, "{}", self.message)
  }
}

impl Diagnostic {
  pub fn display_with_origin<'a>(&'a self, origin: &'a DiagnosticOrigin) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| {
      match origin {
        DiagnosticOrigin::Parsing => writeln!(f, "{self}")?,
        DiagnosticOrigin::Book => writeln!(f, "{self}")?,
        DiagnosticOrigin::Function(nam) => {
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

  /// Transforms a `usize` byte index on `code` into a `TextLocation`.
  pub fn from_byte_loc(code: &str, loc: usize) -> Self {
    let code = code.as_bytes();
    let mut line = 0;
    let mut char = 0;
    let mut cur_idx = 0;
    while cur_idx < loc && cur_idx < code.len() {
      if code[cur_idx] == b'\n' {
        line += 1;
        char = 0;
      } else {
        char += 1;
      }
      cur_idx += 1;
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

  /// Transforms a `usize` byte range on `code` into a `TextLocation`.
  pub fn from_byte_span(code: &str, span: Range<usize>) -> Self {
    // Will loop for way too long otherwise
    assert!(span.start <= span.end);

    let code = code.as_bytes();
    let mut start_line = 0;
    let mut start_char = 0;
    let mut end_line;
    let mut end_char;

    let mut cur_idx = 0;
    while cur_idx < span.start && cur_idx < code.len() {
      if code[cur_idx] == b'\n' {
        start_line += 1;
        start_char = 0;
      } else {
        start_char += 1;
      }
      cur_idx += 1;
    }

    end_line = start_line;
    end_char = start_char;
    while cur_idx < span.end && cur_idx < code.len() {
      if code[cur_idx] == b'\n' {
        end_line += 1;
        end_char = 0;
      } else {
        end_char += 1;
      }
      cur_idx += 1;
    }

    TextSpan::new(TextLocation::new(start_line, start_char), TextLocation::new(end_line, end_char))
  }
}
