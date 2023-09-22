use std::collections::HashMap;

use itertools::Itertools;

/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.
use crate::ast::{hvm_lang::Pattern, DefId, DefinitionBook, Name};

impl DefinitionBook {
  /// Checks whether all rules of a definition have the same number of arguments
  pub fn check_rule_arities(&self) -> anyhow::Result<()> {
    for (&def_id, def) in &self.defs {
      let expected_arity = def.rules[0].pats.len();
      // TODO: Return all errors, don't stop at the first one
      for rule in &def.rules {
        let found_arity = rule.pats.len();
        if expected_arity != found_arity {
          return Err(anyhow::anyhow!(
            "Inconsistent arity on definition '{}'. Expected {} patterns, found {}",
            Name::from(def_id),
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
  pub fn get_types_from_patterns(&self) -> anyhow::Result<HashMap<DefId, Vec<Type>>> {
    type TypeId = usize;
    let mut types: Vec<Type> = vec![];
    let mut pat_to_type: HashMap<(DefId, usize), TypeId> = HashMap::new();
    let mut ctr_name_to_type: HashMap<Name, TypeId> = HashMap::new();
    for (&def_id, def) in &self.defs {
      let mut crnt_types = vec![Type::Any; def.rules[0].pats.len()];
      for rule in &def.rules {
        for (pat_idx, pat) in rule.pats.iter().enumerate() {
          if let Some(pat_type) = Type::from(pat).merge(&crnt_types[pat_idx]) {
            crnt_types[pat_idx] = pat_type;
          } else {
            return Err(todo!());
          }
        }
      }
    }
    todo!()
  }
}

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Number,
  Adt { ctrs: HashMap<Name, Vec<Type>>, others: bool },
}

impl From<&Pattern> for Type {
  fn from(value: &Pattern) -> Self {
    match value {
      Pattern::Ctr(name, args) => Type::Adt {
        ctrs: HashMap::from_iter([(name.clone(), args.into_iter().map(|arg| Type::from(arg)).collect_vec())]),
        others: false,
      },
      Pattern::Num(_) => Type::Number,
      Pattern::Var(_) => Type::Any,
    }
  }
}

impl Type {
  pub fn merge(self, other: &Type) -> Option<Type> {
    todo!()
  }
}
