use core::fmt;
use std::collections::HashMap;

use crate::term::{Book, DefId, Name, RulePat};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Any,
  Adt(Name),
}

pub type DefinitionTypes = HashMap<DefId, Vec<Type>>;

impl Book {
  /// Returns a HashMap from the definition id to the inferred pattern types
  /// and checks the rules arities based on the first rule arity.
  pub fn infer_def_types(&self) -> anyhow::Result<DefinitionTypes> {
    let mut def_types = HashMap::new();

    for def in self.defs.values() {
      self.infer_def_type(def, &mut def_types)?;
    }

    Ok(def_types)
  }

  fn infer_def_type(
    &self,
    def: &crate::term::Definition,
    def_types: &mut HashMap<DefId, Vec<Type>>,
  ) -> Result<(), anyhow::Error> {
    let current_arity = def.arity();
    let mut arg_types = vec![Type::Any; current_arity];
    for rule in def.rules.iter() {
      if rule.arity() != current_arity {
        return Err(anyhow::anyhow!("Arity error."));
      }
      for (idx, pat) in rule.pats.iter().enumerate() {
        match pat {
          RulePat::Var(nam) => match self.ctrs.get(nam) {
            Some(nam) => {
              let t = Type::Adt(nam.clone());
              unify(t, idx, &mut arg_types)?;
            }
            None => {}
          },
          RulePat::Ctr(nam, _) => match self.ctrs.get(nam) {
            Some(nam) => {
              let t = Type::Adt(nam.clone());
              unify(t, idx, &mut arg_types)?;
            }
            None => return Err(anyhow::anyhow!("Unknown constructor '{nam}'.")),
          },
        }
      }
    }
    def_types.insert(def.def_id, arg_types);
    Ok(())
  }
}

fn unify(t: Type, idx: usize, ctx: &mut [Type]) -> Result<(), anyhow::Error> {
  if ctx[idx] == Type::Any {
    ctx[idx] = t;
  } else if ctx[idx] != t.clone() {
    return Err(anyhow::anyhow!("Type mismatch. Found '{}' expected {}.", t, ctx[idx]));
  }
  Ok(())
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Any => write!(f, "any"),
      Type::Adt(nam) => write!(f, "{nam}"),
    }
  }
}
