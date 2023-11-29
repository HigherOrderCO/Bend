use super::type_check::{DefinitionTypes, Type};
use crate::term::{Adt, Book, Name, Pattern, Rule};
use std::collections::BTreeMap;

impl Book {
  /// For each pattern-matching definition, check that any given value will match at least one of the rules.
  pub fn check_exhaustive_patterns(&self, def_types: &DefinitionTypes) -> Result<(), String> {
    for (def_id, def) in &self.defs {
      let def_name = self.def_names.name(def_id).unwrap();
      let types = &def_types[def_id];
      let rules_to_check = Vec::from_iter(0 .. def.rules.len());
      check_pattern(0, &self.adts, &def.rules, types, rules_to_check, def_name)?;
    }
    Ok(())
  }
}

fn check_pattern(
  pat_idx: usize,
  adts: &BTreeMap<Name, Adt>,
  rules: &[Rule],
  types: &[Type],
  rules_to_check: Vec<usize>,
  def_name: &Name,
) -> Result<(), String> {
  if let Some(pat_type) = types.get(0) {
    match pat_type {
      // We can skip non pattern matching arguments
      Type::Any => check_pattern(pat_idx + 1, adts, rules, &types[1 ..], rules_to_check, def_name)?,
      Type::Adt(adt_nam) => {
        let adt = &adts[adt_nam];
        // Find which rules match each constructor
        let mut next_rules_to_check: BTreeMap<Name, Vec<usize>> =
          BTreeMap::from_iter(adt.ctrs.keys().cloned().map(|ctr| (ctr, vec![])));
        for rule_idx in rules_to_check {
          let pat = &rules[rule_idx].pats[pat_idx];
          match pat {
            // Rules with a var pattern are relevant to all constructors.
            Pattern::Var(_) => next_rules_to_check.values_mut().for_each(|x| x.push(rule_idx)),
            Pattern::Ctr(ctr_nam, _) => next_rules_to_check.get_mut(ctr_nam).unwrap().push(rule_idx),
            Pattern::Num(..) => todo!(),
            Pattern::Tup(..) => todo!(),
          }
        }
        // Match each constructor of the current pattern and recursively check the next pattern.
        for (ctr, rules_to_check) in next_rules_to_check {
          if rules_to_check.is_empty() {
            return Err(format!(
              "Non-exhaustive pattern at definition '{}'. Argument {} of type '{}' does not cover the '{}' constructor",
              def_name, pat_idx, adt_nam, ctr
            ));
          } else {
            check_pattern(pat_idx + 1, adts, rules, &types[1 ..], rules_to_check, def_name)?;
          }
        }
      }
      Type::Tup => todo!(),
    }
  }
  Ok(())
}
