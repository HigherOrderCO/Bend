use crate::term::{Book, DefId, Definition, Name, Pattern, Type};
use std::collections::HashMap;

pub type DefinitionTypes = HashMap<DefId, Vec<Type>>;

impl Book {
  /// Returns a HashMap from the definition id to the inferred pattern types
  /// and checks the rules arities based on the first rule arity.
  /// Expects patterns to be flattened.
  pub fn infer_def_types(&self) -> Result<DefinitionTypes, String> {
    let mut def_types = HashMap::new();
    for (def_id, def) in &self.defs {
      let def_type = def
        .infer_type(&self.ctrs)
        .map_err(|e| format!("In definition '{}': {}", self.def_names.name(def_id).unwrap(), e))?;
      def_types.insert(*def_id, def_type);
    }
    Ok(def_types)
  }
}

impl Definition {
  pub fn infer_type(&self, ctrs: &HashMap<Name, Name>) -> Result<Vec<Type>, String> {
    let mut arg_types = vec![];

    for arg_idx in 0 .. self.arity() {
      let pats = self.rules.iter().map(|r| &r.pats[arg_idx]);
      arg_types.push(infer_arg_type(pats, ctrs)?);
    }
    Ok(arg_types)
  }
}

pub fn infer_arg_type<'a>(
  pats: impl Iterator<Item = &'a Pattern>,
  ctrs: &HashMap<Name, Name>,
) -> Result<Type, String> {
  let mut arg_type = Type::Any;
  for pat in pats {
    unify(pat.to_type(ctrs), &mut arg_type)?;
  }
  Ok(arg_type)
}

fn unify(new: Type, old: &mut Type) -> Result<(), String> {
  match (new, &old) {
    (Type::Adt(new), Type::Adt(old)) if &new != old => {
      return Err(format!("Type mismatch. Found '{}' expected {}.", new, old));
    }
    (new, Type::Any) => *old = new,
    _ => (),
  };
  Ok(())
}

impl Book {
  pub fn check_arity(&self) -> Result<(), String> {
    for (def_id, def) in self.defs.iter() {
      def
        .check_arity()
        .map_err(|e| format!("In definition '{}': {}", self.def_names.name(def_id).unwrap(), e))?;
    }
    Ok(())
  }
}

impl Definition {
  pub fn check_arity(&self) -> Result<(), String> {
    let expected_arity = self.arity();
    for rule in &self.rules {
      if rule.arity() != expected_arity {
        return Err(format!("Arity error. Found {} arguments, expected {}", rule.arity(), expected_arity));
      }
    }
    Ok(())
  }
}
