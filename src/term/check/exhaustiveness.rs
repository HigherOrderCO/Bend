use indexmap::IndexMap;

use super::type_check::DefinitionTypes;
use crate::term::{Adt, Book, DefName, MatchNum, Pattern, Rule, Type};

impl Book {
  /// For each pattern-matching definition, check that any given value will match at least one of the rules.
  /// We assume that all patterns have been type checked already.
  pub fn check_exhaustive_patterns(&self, def_types: &DefinitionTypes) -> Result<(), String> {
    for (def_name, def) in &self.defs {
      let types = &def_types[def_name];
      let rules_to_check = (0 .. def.rules.len()).collect();
      check_pattern(&mut vec![], &self.adts, &def.rules, types, rules_to_check, def_name)
        .map_err(|e| format!("In definition '{def_name}': {e}"))?;
    }
    Ok(())
  }
}

fn check_pattern(
  match_path: &mut Vec<DefName>,
  adts: &IndexMap<DefName, Adt>,
  rules: &[Rule],
  types: &[Type],
  rules_to_check: Vec<usize>,
  def_name: &DefName,
) -> Result<(), String> {
  if let Some(pat_type) = types.first() {
    // For each constructor of the pattern, which rules match with it.
    // If no rules match a given constructor, the pattern is non-exhaustive.
    // TODO: Should check if it's a constructor type and use Pattern::is_flat_subset_of.
    let rules_matching_ctrs = match pat_type {
      // We can skip non pattern matching arguments
      Type::Any => IndexMap::from([(DefName::new("_"), rules_to_check)]),
      Type::Adt(adt_nam) => {
        let adt = &adts[adt_nam];
        // For each constructor, which rules do we need to check.
        let mut next_rules_to_check: IndexMap<DefName, Vec<usize>> =
          adt.ctrs.keys().cloned().map(|ctr| (ctr, vec![])).collect();
        for rule_idx in rules_to_check {
          let pat = &rules[rule_idx].pats[match_path.len()];
          match pat {
            // Rules with a var pattern are relevant to all constructors.
            Pattern::Var(_) => next_rules_to_check.values_mut().for_each(|x| x.push(rule_idx)),
            Pattern::Ctr(ctr_nam, _) => next_rules_to_check.get_mut(ctr_nam).unwrap().push(rule_idx),
            // We already type checked, so no other patterns will appear here.
            _ => unreachable!(),
          }
        }
        next_rules_to_check
      }
      Type::Tup => IndexMap::from([(DefName::new("(_,_)"), rules_to_check)]),
      Type::Num => {
        let mut next_rules_to_check: IndexMap<DefName, Vec<usize>> =
          IndexMap::from([(DefName::new("0"), vec![]), (DefName::new("+"), vec![])]);
        for rule_idx in rules_to_check {
          let pat = &rules[rule_idx].pats[match_path.len()];
          match pat {
            Pattern::Var(_) => next_rules_to_check.values_mut().for_each(|x| x.push(rule_idx)),
            Pattern::Num(MatchNum::Zero) => {
              next_rules_to_check.get_mut(&DefName::new("0")).unwrap().push(rule_idx);
            }
            Pattern::Num(MatchNum::Succ { .. }) => {
              next_rules_to_check.get_mut(&DefName::new("+")).unwrap().push(rule_idx);
            }
            _ => unreachable!(),
          }
        }
        next_rules_to_check
      }
      Type::None => unreachable!(),
    };

    // Check that each constructor matches at least one rule and then check the next pattern recursively.
    for (ctr, matching_rules) in rules_matching_ctrs {
      if matching_rules.is_empty() {
        let missing = get_missing_pattern(match_path, &ctr, &types[1 ..], adts);
        return Err(format!("Non-exhaustive pattern. Hint: ({} {}) not covered.", def_name, missing));
      }

      let mut match_path = match_path.clone();
      match_path.push(ctr);
      check_pattern(&mut match_path, adts, rules, &types[1 ..], matching_rules, def_name)?;
    }
  }
  Ok(())
}

/// Returns a string with the first pattern not covered by the definition.
fn get_missing_pattern(
  match_path: &[DefName],
  missing_ctr: &DefName,
  remaining_types: &[Type],
  adts: &IndexMap<DefName, Adt>,
) -> String {
  let mut missing_set: Vec<_> = match_path.iter().map(ToString::to_string).collect();
  missing_set.push(missing_ctr.to_string());
  for typ in remaining_types {
    missing_set.push(first_ctr_of_type(typ, adts));
  }
  missing_set.join(" ")
}

// TODO: Should not reimplement builtins constructor names and instead be in terms of Type::ctrs and Pattern::ctrs.
fn first_ctr_of_type(typ: &Type, adts: &IndexMap<DefName, Adt>) -> String {
  match typ {
    Type::Any => "_".to_string(),
    Type::Tup => "(_,_)".to_string(),
    Type::Num => "0".to_string(),
    Type::Adt(adt_name) => adts[adt_name].ctrs.keys().next().unwrap().to_string(),
    Type::None => unreachable!(),
  }
}
