use crate::{
  diagnostics::WarningType,
  fun::{Book, Ctx, Name, Term},
  maybe_grow,
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Clone, Copy, Debug, PartialEq)]
enum Used {
  /// Definition is accessible from the main entry point, should never be pruned.
  Main,
  /// Definition is not accessible from main, but is accessible from non-builtin definitions.
  NonBuiltin,
  /// Definition is not accessible from main, but is a user-defined constructor.
  Ctr,
}

type Definitions = HashMap<Name, Used>;

impl Ctx<'_> {
  /// If `prune_all`, removes all unused definitions and adts starting from Main.
  /// Otherwise, prunes only the builtins not accessible from any non-built-in definition
  pub fn prune(&mut self, prune_all: bool) {
    let mut used = Definitions::new();

    // Get the functions that are accessible from the main entry point.
    if let Some(main) = &self.book.entrypoint {
      let def = self.book.defs.get(main).unwrap();
      used.insert(main.clone(), Used::Main);
      self.book.find_used_definitions(&def.rule().body, Used::Main, &mut used);
    }

    // Get the functions that are accessible from non-builtins.
    for def in self.book.defs.values() {
      if !def.builtin && !(used.get(&def.name) == Some(&Used::Main)) {
        if self.book.ctrs.contains_key(&def.name) {
          used.insert(def.name.clone(), Used::Ctr);
        } else {
          used.insert(def.name.clone(), Used::NonBuiltin);
        }
        self.book.find_used_definitions(&def.rule().body, Used::NonBuiltin, &mut used);
      }
    }

    // Remove unused definitions.
    for def in self.book.defs.keys().cloned().collect::<Vec<_>>() {
      if let Some(use_) = used.get(&def) {
        match use_ {
          Used::Main => {
            // Used by the main entry point, never pruned;
          }
          Used::NonBuiltin => {
            // Used by a non-builtin definition.
            // Prune if `prune_all`, otherwise show a warning.
            if prune_all {
              self.book.defs.shift_remove(&def);
            } else {
              self.info.add_rule_warning("Definition is unused.", WarningType::UnusedDefinition, def);
            }
          }
          Used::Ctr => {
            // Unused, but a user-defined constructor.
            // Prune if `prune_all`, otherwise nothing.
            if prune_all {
              self.book.defs.shift_remove(&def);
            } else {
              // Don't show warning if it's a user-defined constructor.
            }
          }
        }
      } else {
        // Unused builtin, can always be pruned.
        self.book.defs.shift_remove(&def);
      }
    }
  }
}

impl Book {
  /// Finds all used definitions on every term that can have a def_id.
  fn find_used_definitions(&self, term: &Term, used: Used, uses: &mut Definitions) {
    maybe_grow(|| {
      let mut to_find = vec![term];

      while let Some(term) = to_find.pop() {
        match term {
          Term::Ref { nam: def_name } => self.insert_used(def_name, used, uses),
          Term::List { .. } => {
            self.insert_used(&Name::new(crate::fun::builtins::LCONS), used, uses);
            self.insert_used(&Name::new(crate::fun::builtins::LNIL), used, uses);
          }
          Term::Str { .. } => {
            self.insert_used(&Name::new(crate::fun::builtins::SCONS), used, uses);
            self.insert_used(&Name::new(crate::fun::builtins::SNIL), used, uses);
          }
          _ => {}
        }

        for child in term.children() {
          to_find.push(child);
        }
      }
    })
  }

  fn insert_used(&self, def_name: &Name, used: Used, uses: &mut Definitions) {
    if let Entry::Vacant(e) = uses.entry(def_name.clone()) {
      e.insert(used);

      // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
      // E.g.: the `flatten_rules` golden test
      for rule in &self.defs[def_name].rules {
        self.find_used_definitions(&rule.body, used, uses);
      }
    }
  }
}
