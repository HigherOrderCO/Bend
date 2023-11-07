use std::collections::{BTreeMap, HashSet};

use crate::term::{Book, DefId, Definition, Term};

type Definitions = HashSet<DefId>;

impl Book {
  /// Removes all unused definitions starting from Main.
  pub fn prune(&mut self, main: DefId) {
    let mut used = Definitions::new();

    let Definition { def_id, rules } = self.defs.get(&main).unwrap();
    used.insert(*def_id);
    for rule in rules {
      rule.body.find_used_definitions(&mut used, &self.defs);
    }

    self.defs.retain(|def_id, _| used.contains(def_id));
    self.def_names.map.retain(|def_id, _| used.contains(def_id));
  }
}

impl Term {
  /// Finds all used definitions on every term that can have a def_id.
  fn find_used_definitions(&self, used: &mut Definitions, defs: &BTreeMap<DefId, Definition>) {
    let mut to_visit = vec![self];

    while let Some(term) = to_visit.pop() {
      match term {
        Term::Ref { def_id } => {
          if used.insert(*def_id) {
            let Definition { rules, .. } = defs.get(def_id).unwrap();
            for rule in rules {
              to_visit.push(&rule.body);
            }
          }
        }
        Term::Let { val, nxt, .. } => {
          val.find_used_definitions(used, defs);
          nxt.find_used_definitions(used, defs);
        }
        Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.find_used_definitions(used, defs),
        Term::App { fun, arg } => {
          fun.find_used_definitions(used, defs);
          arg.find_used_definitions(used, defs);
        }
        Term::Match { cond, zero, succ, .. } => {
          cond.find_used_definitions(used, defs);
          zero.find_used_definitions(used, defs);
          succ.find_used_definitions(used, defs);
        }
        Term::Dup { val, nxt, .. } => {
          val.find_used_definitions(used, defs);
          nxt.find_used_definitions(used, defs);
        }
        Term::Sup { fst, snd } => {
          fst.find_used_definitions(used, defs);
          snd.find_used_definitions(used, defs);
        }
        Term::Opx { fst, snd, .. } => {
          fst.find_used_definitions(used, defs);
          snd.find_used_definitions(used, defs);
        }
        Term::Tup { fst, snd } => {
          fst.find_used_definitions(used, defs);
          snd.find_used_definitions(used, defs);
        }
        Term::Var { .. } => (),
        Term::Lnk { .. } => (),
        Term::Era => (),
        Term::Num { .. } => (),
      }
    }
  }
}
