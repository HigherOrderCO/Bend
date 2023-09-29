use hvm_core::Val;
use itertools::Itertools;
use std::collections::HashMap;

/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.
use crate::ast::{hvm_lang::Pattern, DefId, DefinitionBook, Name};

impl DefinitionBook {
  /// Checks whether all rules of a definition have the same number of arguments
  pub fn check_rule_arities(&self) -> anyhow::Result<()> {
    for def in &self.defs {
      let expected_arity = def.rules[0].pats.len();
      // TODO: Return all errors, don't stop at the first one
      for rule in &def.rules {
        let found_arity = rule.pats.len();
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
  pub fn get_types_from_patterns(&self) -> anyhow::Result<(Vec<Adt>, HashMap<DefId, Vec<Type>>)> {
    // For every pattern in every rule in every definition:
    //  Collect all types in pattern
    //  For each collected ADT and their subtypes: (maybe we can flatten first?)
    //   If any constructors have an already used name:
    //    Try to merge this type with the one that was using the same constructor
    //    Update the old definitions to point to the new type
    //    Or, update types that depend on the updated one and update definitions that use it directly
    //    Or, set a redirection system

    let mut adts: Vec<Adt> = vec![];
    let mut pat_to_type: HashMap<(DefId, usize), AdtId> = HashMap::new();
    let mut ctr_name_to_type: HashMap<Name, AdtId> = HashMap::new();
    for def in &self.defs {
      let mut crnt_types = vec![Type::Any; def.rules[0].pats.len()];
      for rule in &def.rules {
        /* for (pat_idx, pat) in rule.pats.iter().enumerate() {
          if let Some(pat_type) = Type::from(pat).merge(&crnt_types[pat_idx]) {
            crnt_types[pat_idx] = pat_type;
          } else {
            return Err(anyhow::anyhow!(
              "Incompatible types in patterns for definition '{}'",
              self.def_names.name(&def.def_id).unwrap()
            ));
          }
        } */
      }
    }
    todo!()
  }
}

type AdtId = Val;

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Adt(AdtId),
  #[cfg(feature = "nums")]
  U32,
  #[cfg(feature = "nums")]
  I32,
}

#[derive(Debug, Clone)]
pub struct Adt {
  ctrs: HashMap<Name, Vec<Type>>,
  others: bool,
}
/*
impl From<&Pattern> for Type {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Ctr(name, args) => Type::Adt {
        ctrs: HashMap::from_iter([(name.clone(), args.into_iter().map(|arg| Type::from(arg)).collect_vec())]),
        others: false,
      },
      Pattern::Var(_) => Type::Any,
      #[cfg(feature = "nums")]
      Pattern::U32(_) => Type::Number,
      #[cfg(feature = "nums")]
      Pattern::UI32(_) => Type::Number,
    }
  }
} */

impl Type {
  pub fn join(&self, other: &Type, adts: &mut Vec<Adt>) -> Option<Type> {
    match (self, other) {
      (Type::Any, Type::Any) => Some(Type::Any),
      (Type::Any, Type::Adt(t)) => Some(Type::Adt(t.clone())),
      (Type::Adt(t), Type::Any) => Some(Type::Adt(t.clone())),
      (Type::Adt(a), Type::Adt(b)) => {
        todo!()
      }
    }
  }

  pub fn merge(&self, other: &Type) -> Option<Type> {
    match (self, other) {
      (Type::Any, Type::Any) => Some(Type::Any),
      (Type::Any, Type::Adt(t)) => Some(Type::Adt(t.clone())),
      (Type::Adt(t), Type::Any) => Some(Type::Adt(t.clone())),
      (Type::Adt(a), Type::Adt(b)) => {
        todo!()
      }
    }
  }
}
