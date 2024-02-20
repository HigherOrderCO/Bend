use crate::term::{
  check::{
    ctrs_arities::ArityErr, exhaustiveness::ExhaustivenessErr, has_entrypoint::EntryErr,
    shared_names::TopLevelErr, type_check::InferErr, unbound_pats::UnboundCtr, unbound_vars::UnboundVar,
  },
  transform::{
    extract_adt_matches::MatchError, resolve_refs::ReferencedMain, simplify_ref_to_ref::ClyclicDef,
  },
  Name,
};
use itertools::Itertools;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Default)]
pub struct Info {
  err_counter: usize,
  pub errs: Vec<Error>,
  pub warnings: Vec<Warning>,
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
  pub fn fatal<T>(&mut self, t: T) -> Result<T, String> {
    if self.errs.len() == self.err_counter { Ok(t) } else { Err(self.take_errs()) }
  }

  pub fn take_errs(&mut self) -> String {
    std::mem::take(&mut self.errs).into_iter().join("\n")
  }
}

// TODO: Merge errors that reference the same definition
#[derive(Debug, Clone)]
pub enum Error {
  Exhaustiveness(Name, ExhaustivenessErr),
  MainRef(Name, ReferencedMain),
  AdtMatch(Name, MatchError),
  UnboundVar(Name, UnboundVar),
  UnboundCtr(Name, UnboundCtr),
  Infer(Name, InferErr),
  Arity(Name, ArityErr),
  Clyclic(ClyclicDef),
  EntryPoint(EntryErr),
  TopLevel(TopLevelErr),
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
      Error::Clyclic(err @ ClyclicDef(def)) => write!(f, "Definition '{def}' {err}"),
      Error::EntryPoint(err) => write!(f, "{err}"),
      Error::TopLevel(err) => write!(f, "{err}"),
    }
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

impl From<ClyclicDef> for Error {
  fn from(value: ClyclicDef) -> Self {
    Self::Clyclic(value)
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
