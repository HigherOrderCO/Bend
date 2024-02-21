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
use std::{
  collections::BTreeMap,
  fmt::{Display, Formatter},
};

pub const INDENT_SIZE: usize = 2;

#[derive(Debug, Clone, Default)]
pub struct Info {
  err_counter: usize,
  errs: Vec<Error>,
  errs_with_def: BTreeMap<Name, Vec<Error>>,
  pub warns: Vec<Warning>,
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
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.errs.iter().join("\n"))?;

    for (def_name, errs) in &self.errs_with_def {
      writeln!(f, "In definition '{def_name}':")?;
      for err in errs {
        writeln!(f, "{:INDENT_SIZE$}{}", "", err)?;
      }
    }

    Ok(())
  }
}

impl From<String> for Info {
  fn from(value: String) -> Self {
    Info {
      err_counter: 0,
      errs: vec![Error::Custom(value)],
      errs_with_def: BTreeMap::new(),
      warns: Vec::new(),
    }
  }
}

impl From<&str> for Info {
  fn from(value: &str) -> Self {
    Info::from(value.to_string())
  }
}

#[derive(Debug, Clone)]
pub enum Error {
  Exhaustiveness(ExhaustivenessErr),
  MainRef(ReferencedMainErr),
  AdtMatch(MatchErr),
  UnboundVar(UnboundVarErr),
  UnboundCtr(UnboundCtrErr),
  Infer(InferErr),
  Arity(ArityErr),
  Cyclic(CyclicDefErr),
  EntryPoint(EntryErr),
  TopLevel(TopLevelErr),
  Custom(String),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Exhaustiveness(err) => write!(f, "{err}"),
      Error::AdtMatch(err) => write!(f, "{err}"),
      Error::UnboundVar(err) => write!(f, "{err}"),
      Error::UnboundCtr(err) => write!(f, "{err}"),
      Error::MainRef(err) => write!(f, "{err}"),
      Error::Infer(err) => write!(f, "{err}"),
      Error::Arity(err) => write!(f, "{err}"),
      Error::Cyclic(err) => write!(f, "{err}"),
      Error::EntryPoint(err) => write!(f, "{err}"),
      Error::TopLevel(err) => write!(f, "{err}"),
      Error::Custom(err) => write!(f, "{err}"),
    }
  }
}

impl From<ExhaustivenessErr> for Error {
  fn from(value: ExhaustivenessErr) -> Self {
    Self::Exhaustiveness(value)
  }
}

impl From<ReferencedMainErr> for Error {
  fn from(value: ReferencedMainErr) -> Self {
    Self::MainRef(value)
  }
}

impl From<MatchErr> for Error {
  fn from(value: MatchErr) -> Self {
    Self::AdtMatch(value)
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

impl From<InferErr> for Error {
  fn from(value: InferErr) -> Self {
    Self::Infer(value)
  }
}

impl From<ArityErr> for Error {
  fn from(value: ArityErr) -> Self {
    Self::Arity(value)
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
