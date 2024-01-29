use super::simplify_ref_to_ref::subst_ref_to_ref;
use crate::term::{Book, DefId, Name, Term};
use std::collections::HashMap;

impl Book {
  /// Merges definitions that have the same structure into one definintion.
  /// Expects variables to be linear.
  pub fn merge_definition(&mut self, main: DefId) {
    let mut term_to_defid: HashMap<Term, DefId> = HashMap::new();
    let mut defid_map: HashMap<DefId, DefId> = HashMap::new();

    for (id, def) in &mut self.defs {
      if id == &main {
        continue;
      }

      def.assert_no_pattern_matching_rules();
      let term = std::mem::take(&mut def.rules[0].body);

      if let Some(new) = term_to_defid.get(&term) {
        defid_map.insert(*id, *new);
      } else {
        term_to_defid.insert(term, *id);
      }
    }

    for (term, id) in term_to_defid {
      self.defs.get_mut(&id).unwrap().rules[0].body = term;
    }

    for (old, new) in &defid_map {
      let old_name = self.def_names.id_to_name.remove(old).unwrap();
      let new_name = self.def_names.name(new).unwrap();

      let merged_name = Name(format!("{}${}", new_name, old_name));

      self.def_names.id_to_name.insert(*new, merged_name.clone());
      self.def_names.name_to_id.insert(merged_name, *new);

      self.def_names.name_to_id.remove(&old_name);
      self.defs.remove(old);
    }

    for def in self.defs.values_mut() {
      subst_ref_to_ref(&mut def.rules[0].body, &defid_map);
    }
  }
}
