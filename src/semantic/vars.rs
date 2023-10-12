use crate::term::{
  transform::{linearize::term_to_affine, unbound_vars::check_uses, unique_names::unique_var_names},
  DefNames, DefinitionBook, Term,
};
use std::collections::HashMap;

/// For all var declarations:
///   If they're used 0 times: erase the declaration
///   If they're used 1 time: leave them as-is
///   If they're used more times: insert dups to make var use affine
/// For all let vars:
///   If they're used 0 times: why? discard the let
///   If they're used 1 time: substitute the body in the var use
///   If they're use more times: add dups for all the uses, put the body at the root dup.
/// For all definition references: Convert from a Var term to an actual Ref term.
/// Reports any unbound variables.
/// Precondition: The pattern matching rules and constructors have already been converted into lambda calculus.

impl DefinitionBook {
  pub fn sanitize_vars(&mut self) -> anyhow::Result<()> {
    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        rule.body = rule.body.sanitize_vars(&self.def_names)?;
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn sanitize_vars(&self, def_names: &DefNames) -> anyhow::Result<Term> {
    check_uses(self, def_names)?;
    let (body, mut var_uses) = unique_var_names(self, def_names)?;
    term_to_affine(body, &mut var_uses, &mut HashMap::new())
  }
}
