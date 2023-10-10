use crate::ast::{DefinitionBook, Name};
use itertools::Itertools;
use std::{collections::HashMap, fmt};

pub mod flatten;
pub mod type_inference;

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
}

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Adt(AdtId),
  U32,
  I32,
}

type AdtId = usize;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Adt {
  // Constructor names and their arities
  ctrs: HashMap<Name, usize>,
  others: bool,
}

impl Adt {
  pub fn from_ctr(nam: Name, arity: usize) -> Self {
    Adt { ctrs: HashMap::from_iter([(nam, arity)]), others: false }
  }

  pub fn new() -> Self {
    Default::default()
  }
}

impl fmt::Display for Adt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "[{}{}]",
      self
        .ctrs
        .iter()
        .sorted()
        .map(|(nam, arity)| format!("({}{})", nam, (0 .. *arity).map(|_| format!(" _")).join("")))
        .join(", "),
      if self.others { ", ..." } else { "" }
    )
  }
}
