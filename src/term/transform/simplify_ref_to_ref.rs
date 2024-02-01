// Pass for inlining functions that are just a reference to another one.

use crate::term::{Book, DefId, Term};
use std::collections::BTreeMap;

impl Book {
  // When we find a function that is simply directly calling another function,
  // substitutes all occurrences of that function to the one being called, avoiding the unnecessary redirect.
  // In case there is a long chain of ref-to-ref-to-ref, we substitute values by the last function in the chain.
  pub fn simplify_ref_to_ref(&mut self) -> Result<(), String> {
    let mut ref_map: BTreeMap<DefId, DefId> = BTreeMap::new();
    // Find to which defs we're mapping the ones that are just references.
    for def_id in self.def_names.def_ids() {
      self.defs[def_id].assert_no_pattern_matching_rules();
      let mut ref_id = def_id;
      let mut is_ref_to_ref = false;
      while let Term::Ref { def_id: next_ref_id } = &self.defs.get(ref_id).unwrap().rules[0].body {
        if next_ref_id == ref_id {
          return Err(format!(
            "Definition {} is a reference to itself",
            self.def_names.name(ref_id).unwrap()
          ));
        }
        ref_id = next_ref_id;
        is_ref_to_ref = true;
      }
      if is_ref_to_ref {
        ref_map.insert(*def_id, *ref_id);
      }
    }

    // Substitute all the occurrences of ref-to-ref.
    for def_id in self.defs.keys().copied().collect::<Vec<_>>() {
      let body = &mut self.defs.get_mut(&def_id).unwrap().rules[0].body;
      // Moving in and out so the borrow checker doesn't complain
      let mut subst_body = std::mem::take(body);
      subst_ref_to_ref(&mut subst_body, &ref_map);
      let body = &mut self.defs.get_mut(&def_id).unwrap().rules[0].body;
      *body = subst_body;
    }

    Ok(())
  }
}

/// Returns whether any substitution happened within the term or not
pub fn subst_ref_to_ref(term: &mut Term, ref_map: &BTreeMap<DefId, DefId>) -> bool {
  match term {
    Term::Ref { def_id } => {
      if let Some(target_id) = ref_map.get(def_id) {
        *def_id = *target_id;
        true
      } else {
        false
      }
    }
    Term::Lam { bod, .. } | Term::Chn { bod, .. } => subst_ref_to_ref(bod, ref_map),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Let { val: fst, nxt: snd, .. }
    | Term::Dup { val: fst, nxt: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      let fst_subst = subst_ref_to_ref(fst, ref_map);
      let snd_subst = subst_ref_to_ref(snd, ref_map);
      fst_subst | snd_subst
    }
    Term::Match { scrutinee, arms } => {
      let mut subst = subst_ref_to_ref(scrutinee, ref_map);
      for (_, term) in arms {
        subst |= subst_ref_to_ref(term, ref_map);
      }

      subst
    }
    Term::List { .. } => unreachable!("Should have been desugared already"),
    Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => false,
  }
}
