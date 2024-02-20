use std::fmt::Display;

use crate::{
  diagnostics::{Error, Info},
  term::{Ctx, Definition, Name, Pattern, Type},
};
use indexmap::IndexMap;

pub type DefinitionTypes = IndexMap<Name, Vec<Type>>;

#[derive(Debug, Clone)]
pub enum InferErr {
  TypeMismatch(Name, Name),
  ArityMismatch(usize, usize),
}

impl Display for InferErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      InferErr::TypeMismatch(new, old) => {
        write!(f, "Type mismatch. Found '{new}' expected {old}.")
      }
      InferErr::ArityMismatch(found, expected) => {
        write!(f, "Arity error. Found {found} arguments, expected {expected}")
      }
    }
  }
}

impl Ctx {
  /// Returns a HashMap from the definition id to the inferred pattern types
  /// and checks the rules arities based on the first rule arity.
  /// Expects patterns to be flattened.
  pub fn infer_def_types(&mut self) -> Result<DefinitionTypes, Info> {
    self.info.start_pass();

    let mut def_types = IndexMap::new();

    for (def_name, def) in &self.book.defs {
      match def.infer_type(&self.book.ctrs) {
        Ok(def_type) => _ = def_types.insert(def_name.clone(), def_type),
        Err(e) => self.info.error(e),
      }
    }

    self.info.fatal(def_types)
  }
}

impl Definition {
  pub fn infer_type(&self, ctrs: &IndexMap<Name, Name>) -> Result<Vec<Type>, Error> {
    let mut arg_types = vec![];

    for arg_idx in 0 .. self.arity() {
      let pats = self.rules.iter().map(|r| &r.pats[arg_idx]);
      let value = infer_arg_type(pats, ctrs).map_err(|e| Error::Infer(self.name.clone(), e))?;
      arg_types.push(value);
    }
    Ok(arg_types)
  }
}

pub fn infer_arg_type<'a>(
  pats: impl Iterator<Item = &'a Pattern>,
  ctrs: &IndexMap<Name, Name>,
) -> Result<Type, InferErr> {
  let mut arg_type = Type::Any;
  for pat in pats {
    unify(pat.to_type(ctrs), &mut arg_type)?;
  }
  Ok(arg_type)
}

fn unify(new: Type, old: &mut Type) -> Result<(), InferErr> {
  match (new, &old) {
    (Type::Adt(new), Type::Adt(old)) if &new != old => {
      return Err(InferErr::TypeMismatch(new, old.clone()));
    }
    (new, Type::Any) => *old = new,
    _ => (),
  };
  Ok(())
}

impl Ctx {
  pub fn check_arity(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter() {
      if let Err(e) = def.check_arity() {
        self.info.error(Error::Infer(def_name.clone(), e))
      };
    }

    self.info.fatal(())
  }
}

impl Definition {
  pub fn check_arity(&self) -> Result<(), InferErr> {
    let expected_arity = self.arity();
    for rule in &self.rules {
      if rule.arity() != expected_arity {
        return Err(InferErr::ArityMismatch(rule.arity(), expected_arity));
      }
    }
    Ok(())
  }
}
