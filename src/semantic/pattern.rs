use crate::ast::{
  hvm_lang::{DefNames, Pattern},
  DefId, Definition, DefinitionBook, Name,
};
use hvm_core::Val;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.

impl DefinitionBook {
  /// Checks whether all rules of a definition have the same number of arguments
  pub fn check_rule_arities(&self) -> anyhow::Result<()> {
    for def in &self.defs {
      let expected_arity = def.arity();
      // TODO: Return all errors, don't stop at the first one
      for rule in &def.rules {
        let found_arity = rule.arity();
        if expected_arity != found_arity {
          return Err(anyhow::anyhow!(
            "Inconsistent arity on definition '{}'. Expected {} patterns, found {}",
            self.def_names.name(&def.def_id).unwrap(),
            expected_arity,
            found_arity
          ));
        }
      }
    }
    Ok(())
  }

  /// Infers ADTs from the patterns of the rules in a book.
  /// Returns the infered type of the patterns of each definition.
  /// Returns an error if rules use the types in a inconsistent way.
  /// These could be same name in different types, different arities or mixing numbers and ADTs.
  /// Precondition: Rules have been flattened, rule arity is correct.
  pub fn get_types_from_patterns(&self) -> anyhow::Result<(Vec<Adt>, HashMap<DefId, Vec<Type>>)> {
    // For every pattern in every rule in every definition:
    //  Collect all types in pattern
    //  For each collected ADT and their subtypes: (maybe we can flatten first?)
    //   If any constructors have an already used name:
    //    Try to merge this type with the one that was using the same constructor
    //    Update the old definitions to point to the new type
    //    Or, update types that depend on the updated one and update definitions that use it directly
    //    Or, set a redirection system

    let mut adts: Vec<Rc<RefCell<Adt>>> = vec![];
    let mut types: Vec<Vec<Type>> = vec![]; // The type of each 
    let mut ctr_name_to_adt: HashMap<Name, usize> = HashMap::new();
    for def in &self.defs {
      let mut pat_types = get_types_from_def_patterns(def, &self.def_names)?;
      // Check if the types in this def share some ctr names with previous types.
      // Try to merge them if they do
      for typ in pat_types.clone() {
        if let Type::Adt(crnt_adt) = typ {
          for ctr_name in crnt_adt.borrow().ctrs.keys() {
            if let Some(&old_adt_idx) = ctr_name_to_adt.get(ctr_name) {
              if crnt_adt.borrow_mut().merge(&adts[old_adt_idx].borrow()) {
                // TODO: Make both point at the same adt somehow?
              } else {
                // TODO: Differentiate between wrong arity and different adts infered for same name.
                return Err(anyhow::anyhow!(
                  "Inconsistent use of constructor '{}' on definition '{}'.",
                  ctr_name,
                  self.def_names.name(&def.def_id).unwrap(),
                ));
              }
            }
          }
        }
      }
      types.push(pat_types);
    }
    todo!()
  }
}

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Adt(Rc<RefCell<Adt>>),
  #[cfg(feature = "nums")]
  U32,
  #[cfg(feature = "nums")]
  I32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Adt {
  // Constructor names and their arities
  ctrs: HashMap<Name, usize>,
  others: bool,
}

impl From<&Pattern> for Type {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Ctr(name, args) => Type::Adt(Rc::new(
        Adt { ctrs: HashMap::from_iter([(name.clone(), args.len())]), others: false }.into(),
      )),
      Pattern::Var(_) => Type::Any,
      #[cfg(feature = "nums")]
      Pattern::U32(_) => Type::Number,
      #[cfg(feature = "nums")]
      Pattern::UI32(_) => Type::Number,
    }
  }
}

impl Type {
  pub fn join(&self, other: &Type) -> Option<Type> {
    match (self, other) {
      (Type::Any, Type::Any) => Some(Type::Any),
      (Type::Any, Type::Adt(t)) => Some(Type::Adt(t.clone())),
      (Type::Adt(adt), Type::Any) => {
        adt.borrow_mut().others = true;
        Some(Type::Adt(adt.clone()))
      }
      (Type::Adt(adt_a), Type::Adt(adt_b)) => {
        todo!()
      }
    }
  }
}

impl Adt {
  pub fn merge(&mut self, other: &Adt) -> bool {
    // TODO: Don't copy these names so much
    let accept_different = self.others || other.others;
    if accept_different {
      self.others = accept_different;
      for (ctr_name, ctr_arity) in &other.ctrs {
        if let Some(old_arity) = self.ctrs.insert(ctr_name.clone(), *ctr_arity) {
          if old_arity != *ctr_arity {
            return false;
          }
        }
      }
      true
    } else {
      self == other
    }
  }
}

fn get_types_from_def_patterns(def: &Definition, def_names: &DefNames) -> anyhow::Result<Vec<Type>> {
  let arity = def.arity();
  let mut pat_types = vec![];
  for pat_idx in 0 .. arity {
    let pats = def.rules.iter().map(|x| &x.pats[pat_idx]);
    let mut pat_type = Type::Any;
    for pat in pats {
      if let Some(t) = pat_type.join(&Type::from(pat)) {
        pat_type = t;
      } else {
        // TODO: Improve error reporting.
        return Err(anyhow::anyhow!(
          "Incompatible types in patterns for definition '{}'",
          def_names.name(&def.def_id).unwrap()
        ));
      }
    }
    pat_types.push(pat_type);
  }

  Ok(pat_types)
}
