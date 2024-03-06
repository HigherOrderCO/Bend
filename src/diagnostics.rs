use crate::term::{
  check::{
    repeated_bind::RepeatedBindWarn, set_entrypoint::EntryErr, shared_names::TopLevelErr,
    unbound_pats::UnboundCtrErr, unbound_vars::UnboundVarErr,
  },
  display::DisplayFn,
  transform::{
    apply_args::ArgError, encode_pattern_matching::MatchErr, resolve_refs::ReferencedMainErr,
    simplify_ref_to_ref::CyclicDefErr,
  },
  Name,
};
use itertools::Itertools;
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter},
};

pub const ERR_INDENT_SIZE: usize = 2;

#[derive(Debug, Clone, Default)]
pub struct Info {
  err_counter: usize,
  errs: Vec<Error>,
  errs_with_def: BTreeMap<Name, Vec<Error>>,
  pub warns: Warnings,
}

impl Info {
  pub fn error<E: Into<Error>>(&mut self, e: E) {
    self.err_counter += 1;
    self.errs.push(e.into())
  }

  pub fn def_error<E: Into<Error>>(&mut self, name: Name, e: E) {
    self.err_counter += 1;
    let entry = self.errs_with_def.entry(name).or_default();
    entry.push(e.into());
  }

  pub fn take_err<T, E: Into<Error>>(&mut self, result: Result<T, E>, def_name: Option<&Name>) -> Option<T> {
    match result {
      Ok(t) => Some(t),
      Err(e) => {
        match def_name {
          None => self.error(e),
          Some(def) => self.def_error(def.clone(), e),
        }
        None
      }
    }
  }

  pub fn has_errors(&self) -> bool {
    !(self.errs.is_empty() && self.errs_with_def.is_empty())
  }

  /// Resets the internal counter
  pub fn start_pass(&mut self) {
    self.err_counter = 0;
  }

  /// Checks if any error was emitted since the start of the pass,
  /// Returning all the current information as a `Err(Info)`, replacing `&mut self` with an empty one.
  /// Otherwise, returns the given arg as an `Ok(T)`.
  pub fn fatal<T>(&mut self, t: T) -> Result<T, Info> {
    if self.err_counter == 0 { Ok(t) } else { Err(std::mem::take(self)) }
  }

  pub fn warning<W: Into<WarningType>>(&mut self, def_name: Name, warning: W) {
    self.warns.0.entry(def_name).or_default().push(warning.into());
  }

  pub fn display(&self, verbose: bool) -> impl Display + '_ {
    DisplayFn(move |f| {
      writeln!(f, "{}", self.errs.iter().map(|err| err.display(verbose)).join("\n"))?;

      for (def_name, errs) in &self.errs_with_def {
        in_definition(def_name, f)?;
        for err in errs {
          writeln!(f, "{:ERR_INDENT_SIZE$}{}", "", err.display(verbose))?;
        }
      }

      Ok(())
    })
  }
}

fn in_definition(def_name: &Name, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
  writeln!(f, "In definition '{def_name}':")
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display(false))
  }
}

impl From<String> for Info {
  fn from(value: String) -> Self {
    Info { errs: vec![Error::Custom(value)], ..Default::default() }
  }
}

impl From<&str> for Info {
  fn from(value: &str) -> Self {
    Info::from(value.to_string())
  }
}

#[derive(Debug, Clone)]
pub enum Error {
  MainRef(ReferencedMainErr),
  Match(MatchErr),
  UnboundVar(UnboundVarErr),
  UnboundCtr(UnboundCtrErr),
  Cyclic(CyclicDefErr),
  EntryPoint(EntryErr),
  TopLevel(TopLevelErr),
  Custom(String),
  ArgError(ArgError),
  RepeatedBind(RepeatedBindWarn),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display(false))
  }
}

impl Error {
  pub fn display(&self, verbose: bool) -> impl Display + '_ {
    DisplayFn(move |f| match self {
      Error::Match(err) => write!(f, "{}", err.display(verbose)),
      Error::UnboundVar(err) => write!(f, "{err}"),
      Error::UnboundCtr(err) => write!(f, "{err}"),
      Error::MainRef(err) => write!(f, "{err}"),
      Error::Cyclic(err) => write!(f, "{err}"),
      Error::EntryPoint(err) => write!(f, "{err}"),
      Error::TopLevel(err) => write!(f, "{err}"),
      Error::Custom(err) => write!(f, "{err}"),
      Error::ArgError(err) => write!(f, "{err}"),
      Error::RepeatedBind(err) => write!(f, "{err}"),
    })
  }
}

impl From<ReferencedMainErr> for Error {
  fn from(value: ReferencedMainErr) -> Self {
    Self::MainRef(value)
  }
}

impl From<MatchErr> for Error {
  fn from(value: MatchErr) -> Self {
    Self::Match(value)
  }
}

impl From<UnboundVarErr> for Error {
  fn from(value: UnboundVarErr) -> Self {
    Self::UnboundVar(value)
  }
}

impl From<UnboundCtrErr> for Error {
  fn from(value: UnboundCtrErr) -> Self {
    Self::UnboundCtr(value)
  }
}

impl From<CyclicDefErr> for Error {
  fn from(value: CyclicDefErr) -> Self {
    Self::Cyclic(value)
  }
}

impl From<EntryErr> for Error {
  fn from(value: EntryErr) -> Self {
    Self::EntryPoint(value)
  }
}

impl From<TopLevelErr> for Error {
  fn from(value: TopLevelErr) -> Self {
    Self::TopLevel(value)
  }
}

impl From<ArgError> for Error {
  fn from(value: ArgError) -> Self {
    Self::ArgError(value)
  }
}

#[derive(Debug, Clone, Default)]
pub struct Warnings(pub BTreeMap<Name, Vec<WarningType>>);

#[derive(Debug, Clone)]
pub enum WarningType {
  MatchOnlyVars,
  UnusedDefinition,
  RepeatedBind(RepeatedBindWarn),
}

impl Display for WarningType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      WarningType::MatchOnlyVars => write!(f, "Match expression at definition only uses var patterns."),
      WarningType::UnusedDefinition => write!(f, "Definition is unused."),
      WarningType::RepeatedBind(warn) => write!(f, "{warn}"),
    }
  }
}

impl Display for Warnings {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    for (def_name, warns) in &self.0 {
      in_definition(def_name, f)?;
      for warn in warns {
        writeln!(f, "{:ERR_INDENT_SIZE$}{}", "", warn)?;
      }
    }

    Ok(())
  }
}

impl From<RepeatedBindWarn> for WarningType {
  fn from(value: RepeatedBindWarn) -> Self {
    Self::RepeatedBind(value)
  }
}
