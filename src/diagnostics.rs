use crate::term::{
  check::{
    ctrs_arities::ArityErr, exhaustiveness::ExhaustivenessErr, set_entrypoint::EntryErr,
    shared_names::TopLevelErr, type_check::InferErr, unbound_pats::UnboundCtrErr,
    unbound_vars::UnboundVarErr,
  },
  transform::{
    extract_adt_matches::MatchErr, resolve_refs::ReferencedMainErr, simplify_ref_to_ref::CyclicDefErr,
  },
  Name,
};
use itertools::Itertools;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Default)]
pub struct Info {
  err_counter: usize,
  pub errs: Vec<Error>,
  pub warns: Vec<Warning>,
}

impl Info {
  pub fn error<E: Into<Error>>(&mut self, e: E) {
    self.errs.push(e.into())
  }

  /// Updates the internal counter to the current number of errors
  pub fn start_pass(&mut self) {
    self.err_counter = self.errs.len();
  }

  /// Checks if any error was emitted since the start of the pass,  
  /// Returning all the current errors as a `Err(String)`.  
  /// Otherwise, returns the given arg as an `Ok(T)`.
  pub fn fatal<T>(&mut self, t: T) -> Result<T, Info> {
    if self.errs.len() == self.err_counter { Ok(t) } else { Err(std::mem::take(self)) }
  }
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.errs.iter().join("\n"))
  }
}

impl From<String> for Info {
  fn from(value: String) -> Self {
    Info { err_counter: 0, errs: vec![Error::Custom(value)], warns: Vec::new() }
  }
}

impl From<&str> for Info {
  fn from(value: &str) -> Self {
    Info::from(value.to_string())
  }
}

// TODO: Merge errors that reference the same definition
#[derive(Debug, Clone)]
pub enum Error {
  Exhaustiveness(Name, ExhaustivenessErr),
  MainRef(Name, ReferencedMainErr),
  AdtMatch(Name, MatchErr),
  UnboundVar(Name, UnboundVarErr),
  UnboundCtr(Name, UnboundCtrErr),
  Infer(Name, InferErr),
  Arity(Name, ArityErr),
  Cyclic(CyclicDefErr),
  EntryPoint(EntryErr),
  TopLevel(TopLevelErr),
  Custom(String),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Exhaustiveness(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::AdtMatch(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::UnboundVar(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::UnboundCtr(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::MainRef(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::Infer(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::Arity(def_name, err) => write!(f, "In definition '{def_name}': {err}"),
      Error::Cyclic(err @ CyclicDefErr(def)) => write!(f, "Definition '{def}' {err}"),
      Error::EntryPoint(err) => write!(f, "{err}"),
      Error::TopLevel(err) => write!(f, "{err}"),
      Error::Custom(err) => write!(f, "{err}"),
    }
  }
}

impl From<EntryErr> for Error {
  fn from(value: EntryErr) -> Self {
    Self::EntryPoint(value)
  }
}

impl From<CyclicDefErr> for Error {
  fn from(value: CyclicDefErr) -> Self {
    Self::Cyclic(value)
  }
}

#[derive(Debug, Clone)]
pub enum Warning {
  MatchOnlyVars(Name),
  UnusedDefinition(Name),
}

impl Display for Warning {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Warning::MatchOnlyVars(def_name) => {
        write!(f, "Match expression at definition '{def_name}' only uses var patterns.")
      }
      Warning::UnusedDefinition(def_name) => write!(f, "Unused definition '{def_name}'."),
    }
  }
}
