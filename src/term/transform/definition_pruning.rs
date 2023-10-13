use std::collections::{BTreeMap, HashSet};

use crate::term::{DefId, Definition, DefinitionBook, Name, Term};

type Definitions = HashSet<DefId>;

impl DefinitionBook {
  /// Removes all unused definitions starting from Main.
  pub fn prune(&mut self) {
    if let Some(ref main) = self.def_names.def_id(&Name::new("Main")) {
      let mut used = Definitions::new();

      let Definition { def_id, body } = self.defs.get(main).unwrap();
      used.insert(*def_id);
      body.find_used_definitions(&mut used, &self.defs);

      self.defs.retain(|def_id, _| used.contains(def_id));
      self.def_names.map.retain(|def_id, _| used.contains(def_id));
    }
  }
}

impl Term {
  /// Recursively finds all used definitions on every term that can have a def_id.
  fn find_used_definitions(&self, used: &mut Definitions, defs: &BTreeMap<DefId, Definition>) {
    match &self {
      Term::Ref { def_id } => {
        if used.insert(*def_id) {
          let Definition { body, .. } = defs.get(def_id).unwrap();
          body.find_used_definitions(used, defs);
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
      _ => {}
    }
  }
}
