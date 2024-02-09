use super::simplify_ref_to_ref::subst_ref_to_ref;
use crate::term::{Book, Definition, Name, Origin, Rule, Term};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use std::collections::BTreeMap;

impl Book {
  /// Merges definitions that have the same structure into one definition.
  /// Expects variables to be linear.
  ///
  /// Ignores origin of the rules when merging,
  /// Should not be preceded by passes that cares about the origins.
  pub fn merge_definitions(&mut self, main: &Name) {
    let defs: Vec<_> = self.defs.keys().cloned().collect();
    self.merge(main, defs.into_iter());
  }

  /// Checks and merges identical definitions given by `defs`.
  /// We never merge the entrypoint function with something else.
  fn merge(&mut self, main: &Name, defs: impl Iterator<Item = Name>) {
    // Sets of definitions that are identical, indexed by the body term.
    let equal_terms = self.collect_terms(defs.filter(|def_name| def_name != main));

    // Map of old name to new merged name
    let mut name_map = BTreeMap::new();

    for (term, equal_defs) in equal_terms {
      // Create the merged name
      let new_name = Name::from(equal_defs.iter().join("_$_"));

      // Write the mapping of old to new names (only if something was merged)
      if equal_defs.len() > 1 {
        for name in equal_defs {
          name_map.insert(name, new_name.clone());
        }
      }

      // Create the merged function
      let new_def = Definition {
        name: new_name.clone(),
        rules: vec![Rule { pats: vec![], body: term, origin: Origin::Generated }],
      };
      self.defs.insert(new_name, new_def);
    }
    self.update_refs(&name_map, main);
  }

  fn collect_terms(&mut self, def_entries: impl Iterator<Item = Name>) -> IndexMap<Term, IndexSet<Name>> {
    let mut equal_terms: IndexMap<Term, IndexSet<Name>> = IndexMap::new();

    for def_name in def_entries {
      let mut def = self.defs.remove(&def_name).unwrap();
      let term = std::mem::take(&mut def.rule_mut().body);
      equal_terms.entry(term).or_default().insert(def_name);
    }

    equal_terms
  }

  fn update_refs(&mut self, name_map: &BTreeMap<Name, Name>, main: &Name) {
    let mut updated_defs = Vec::new();

    for def in self.defs.values_mut() {
      if subst_ref_to_ref(&mut def.rule_mut().body, name_map) {
        updated_defs.push(def.name.clone());
      }
    }

    if !updated_defs.is_empty() {
      self.merge(main, updated_defs.into_iter());
    }
  }
}
