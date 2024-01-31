use super::simplify_ref_to_ref::subst_ref_to_ref;
use crate::term::{Book, DefId, Name, Term};
use std::collections::HashMap;

impl Book {
  /// Merges definitions that have the same structure into one definition.
  /// Expects variables to be linear.
  pub fn merge_definitions(&mut self, main: DefId) {
    let ids: Vec<_> = self.defs.keys().copied().collect();
    let filtered_ids = ids.into_iter().filter(|&id| id != main);

    self.merge(main, filtered_ids);
  }

  fn merge(&mut self, main: DefId, defs: impl Iterator<Item = DefId>) {
    let mut term_map: HashMap<Term, DefId> = HashMap::new();
    let mut def_id_map: HashMap<DefId, DefId> = HashMap::new();

    self.collect_terms(defs, &mut term_map, &mut def_id_map);
    self.merge_terms(main, term_map, def_id_map);
  }

  fn collect_terms<'a>(
    &mut self,
    def_entries: impl Iterator<Item = DefId>,
    term_map: &mut HashMap<Term, DefId>,
    def_id_map: &mut HashMap<DefId, DefId>,
  ) {
    for id in def_entries {
      let def = self.defs.get_mut(&id).unwrap();

      def.assert_no_pattern_matching_rules();
      let term = std::mem::take(&mut def.rules[0].body);

      if let Some(new) = term_map.get(&term) {
        def_id_map.insert(id, *new);
      } else {
        def_id_map.insert(id, id);
        term_map.insert(term, id);
      }
    }
  }

  fn merge_terms(&mut self, main: DefId, term_map: HashMap<Term, DefId>, def_id_map: HashMap<DefId, DefId>) {
    for (term, id) in term_map {
      self.defs.get_mut(&id).unwrap().rules[0].body = term;
    }

    self.merge_names(&def_id_map);
    self.update_refs(def_id_map, main);
  }

  fn merge_names(&mut self, def_id_map: &HashMap<DefId, DefId>) {
    for (old, new) in def_id_map {
      if old == new {
        continue;
      }

      let old_name = self.def_names.name(old).unwrap();
      let new_name = self.def_names.name(new).unwrap();

      let merged_name = Name(format!("{}=$={}", new_name, old_name));

      self.def_names.id_to_name.insert(*new, merged_name.clone());
      self.def_names.name_to_id.insert(merged_name, *new);

      self.remove_def(*old);
    }
  }

  fn update_refs(&mut self, def_id_map: HashMap<DefId, DefId>, main: DefId) {
    let mut updated_defs = Vec::new();

    for def in self.defs.values_mut() {
      if subst_ref_to_ref(&mut def.rules[0].body, &def_id_map) {
        if def.def_id != main {
          updated_defs.push(def.def_id);
        }
      }
    }

    if !updated_defs.is_empty() {
      self.merge(main, updated_defs.into_iter());
    }
  }
}
