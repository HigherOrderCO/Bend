/// Semantic passes for pattern matching on defiinition rules.
/// Extract ADTs from patterns in a book, then convert them into lambda calculus.
use crate::ast::DefinitionBook;

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
