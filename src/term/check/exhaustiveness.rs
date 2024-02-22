use super::type_check::DefinitionTypes;
use crate::{
  diagnostics::{Info, ERR_INDENT_SIZE},
  term::{Adt, Ctx, MatchNum, Name, Pattern, Rule, Type},
};
use indexmap::IndexMap;
use itertools::Itertools;
use std::fmt::Display;

const PATTERN_ERROR_LIMIT: usize = 5;
const ERROR_LIMIT_HINT: &str = "Use the --verbose option to see all cases.";

#[derive(Debug, Clone)]
pub struct ExhaustivenessErr(Name, Vec<String>);

impl ExhaustivenessErr {
  pub fn display_with_limit(&self, limit: usize) -> String {
    let ident = ERR_INDENT_SIZE * 2;
    let hints =
      self.1.iter().take(limit).map(|pat| format!("{:ident$}({} {pat}) not covered.", "", self.0)).join("\n");

    let mut str = format!("Non-exhaustive pattern. Hint:\n{}", hints);

    let len = self.1.len();
    if len > limit {
      str.push_str(&format!(" ... and {} others.\n{:ident$}{}", len - limit, "", ERROR_LIMIT_HINT))
    }

    str
  }
}

impl Display for ExhaustivenessErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display_with_limit(PATTERN_ERROR_LIMIT))
  }
}

impl<'book> Ctx<'book> {
  /// For each pattern-matching definition, check that any given value will match at least one of the rules.
  /// We assume that all patterns have been type checked already.
  pub fn check_exhaustive_patterns(&mut self, def_types: &DefinitionTypes) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in &self.book.defs {
      let types = &def_types[def_name];
      let rules_to_check = (0 .. def.rules.len()).collect();
      let res = check_pattern(&[], &self.book.adts, &def.rules, types, rules_to_check, def_name);
      self.info.take_err(res, Some(&def_name));
    }

    self.info.fatal(())
  }
}

fn check_pattern(
  match_path: &[Name],
  adts: &IndexMap<Name, Adt>,
  rules: &[Rule],
  types: &[Type],
  rules_to_check: Vec<usize>,
  def_name: &Name,
) -> Result<(), ExhaustivenessErr> {
  let mut missings = Vec::new();

  if let Some(pat_type) = types.first() {
    // For each constructor of the pattern, which rules match with it.
    // If no rules match a given constructor, the pattern is non-exhaustive.
    // TODO: Should check if it's a constructor type and use Pattern::is_flat_subset_of.
    let rules_matching_ctrs = match pat_type {
      // We can skip non pattern matching arguments
      Type::Any => IndexMap::from([(Name::from("_"), rules_to_check)]),
      Type::Adt(adt_nam) => {
        let adt = &adts[adt_nam];
        // For each constructor, which rules do we need to check.
        let mut next_rules_to_check: IndexMap<Name, Vec<usize>> =
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
      Type::Tup => IndexMap::from([(Name::from("(_,_)"), rules_to_check)]),
      Type::Num => {
        let mut next_rules_to_check: IndexMap<Name, Vec<usize>> =
          IndexMap::from([(Name::from("0"), vec![]), (Name::from("+"), vec![])]);
        for rule_idx in rules_to_check {
          let pat = &rules[rule_idx].pats[match_path.len()];
          match pat {
            Pattern::Var(_) => next_rules_to_check.values_mut().for_each(|x| x.push(rule_idx)),
            Pattern::Num(MatchNum::Zero) => {
              next_rules_to_check.get_mut(&Name::from("0")).unwrap().push(rule_idx);
            }
            Pattern::Num(MatchNum::Succ { .. }) => {
              next_rules_to_check.get_mut(&Name::from("+")).unwrap().push(rule_idx);
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
        missings.push(missing);
      }

      let mut match_path = match_path.to_owned();
      match_path.push(ctr);
      check_pattern(&match_path, adts, rules, &types[1 ..], matching_rules, def_name)?;
    }
  }

  if missings.is_empty() { Ok(()) } else { Err(ExhaustivenessErr(def_name.clone(), missings)) }
}

/// Returns a string with the first pattern not covered by the definition.
fn get_missing_pattern(
  match_path: &[Name],
  missing_ctr: &Name,
  remaining_types: &[Type],
  adts: &IndexMap<Name, Adt>,
) -> String {
  let mut missing_set: Vec<_> = match_path.iter().map(ToString::to_string).collect();
  missing_set.push(missing_ctr.to_string());
  for typ in remaining_types {
    missing_set.push(first_ctr_of_type(typ, adts));
  }
  missing_set.join(" ")
}

// TODO: Should not reimplement builtins constructor names and instead be in terms of Type::ctrs and Pattern::ctrs.
fn first_ctr_of_type(typ: &Type, adts: &IndexMap<Name, Adt>) -> String {
  match typ {
    Type::Any => "_".to_string(),
    Type::Tup => "(_,_)".to_string(),
    Type::Num => "0".to_string(),
    Type::Adt(adt_name) => adts[adt_name].ctrs.keys().next().unwrap().to_string(),
    Type::None => unreachable!(),
  }
}
