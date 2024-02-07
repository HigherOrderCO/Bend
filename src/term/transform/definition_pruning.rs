use std::collections::{hash_map::Entry, HashMap};

use crate::{
  term::{Adt, Book, DefName, Origin, Tag, Term},
  Warning,
};
use indexmap::IndexSet;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Used {
  /// Rule is a constructor for an Adt
  /// If not `prune_all`, should not be pruned when:
  /// It or its Adt as a tag name are referenced in the user code at all (I.e. non-built-in code)
  Adt,

  /// Rule is accessible from the main entry point, should never be pruned
  Main,

  /// Rule is not accessible from main nor it's an Adt
  /// Should be pruned if `prune_all` or if it's a built-in rule
  /// Otherwise, if the rule has a non-generated name, a warning about the unused def is returned
  Unused,
}

type Definitions = HashMap<DefName, Used>;

impl Book {
  /// If `prune_all`, removes all unused definitions and adts starting from Main.
  /// Otherwise, prunes only the builtins not accessible from any non-built-in definition
  pub fn prune(&mut self, main: Option<&DefName>, prune_all: bool, warnings: &mut Vec<Warning>) {
    let mut used = Definitions::new();

    if let Some(main) = main {
      let def = self.defs.get(main).unwrap();
      used.insert(main.clone(), Used::Main);
      self.find_used_definitions(&def.rule().body, Used::Main, &mut used);
    }

    // Even if we don't prune all the defs, we need check what built-ins are accessible through user code
    if !prune_all {
      for (def_name, def) in &self.defs {
        // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
        // E.g.: the `flatten_rules` golden test
        for rule in &def.rules {
          if rule.origin != Origin::Builtin {
            match used.entry(def_name.clone()) {
              Entry::Vacant(e) => _ = e.insert(Used::Unused),
              Entry::Occupied(e) if *e.get() != Used::Unused => continue,
              _ => {}
            }

            self.find_used_definitions(&rule.body, Used::Unused, &mut used);
          }
        }
      }
    }

    let names = self.defs.keys().cloned().collect::<IndexSet<DefName>>();

    // Filter defs from the 'used' hashmap that are not accessible from main
    let filter = |(name, used)| if used == Used::Unused { None } else { Some(name) };
    let used: IndexSet<DefName> = used.into_iter().filter_map(filter).collect();

    let unused = names.difference(&used).cloned();

    self.prune_unused(unused, prune_all, warnings);
  }

  fn prune_unused(
    &mut self,
    unused: impl IntoIterator<Item = DefName>,
    prune_all: bool,
    warnings: &mut Vec<Warning>,
  ) {
    for def_name in unused {
      let def = &self.defs[&def_name];
      if prune_all || def.is_builtin() {
        self.defs.remove(&def_name);
      } else if !def_name.is_generated() {
        warnings.push(Warning::UnusedDefinition { def_name: def_name.clone() });
      }
    }
  }

  /// Finds all used definitions on every term that can have a def_id.
  fn find_used_definitions(&self, term: &Term, used: Used, uses: &mut Definitions) {
    match term {
      Term::Ref { def_name } => match self.ctrs.get(def_name) {
        Some(name) => self.insert_ctrs_used(name, uses),
        None => self.insert_used(def_name, used, uses),
      },

      Term::Lam { tag: Tag::Named(name), bod, .. } | Term::Chn { tag: Tag::Named(name), bod, .. } => {
        // mark constructors of the Adt with same name as the tag in case of the user
        // manually encoded a constructor with tagged lambdas
        self.insert_ctrs_used(name, uses);
        self.find_used_definitions(bod, used, uses);
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => self.find_used_definitions(bod, used, uses),
      Term::Let { val, nxt, .. } | Term::Dup { val, nxt, .. } => {
        self.find_used_definitions(val, used, uses);
        self.find_used_definitions(nxt, used, uses);
      }
      Term::App { tag, fun, arg } => {
        // mark constructors of the Adt with same name as the tag in case of the user
        // manually encoded a pattern matching with tagged applications
        if let Tag::Named(name) = tag {
          self.insert_ctrs_used(name, uses);
        }

        self.find_used_definitions(fun, used, uses);
        self.find_used_definitions(arg, used, uses);
      }
      Term::Sup { fst, snd, .. } | Term::Tup { fst, snd } | Term::Opx { fst, snd, .. } => {
        self.find_used_definitions(fst, used, uses);
        self.find_used_definitions(snd, used, uses);
      }
      Term::Match { scrutinee, arms } => {
        self.find_used_definitions(scrutinee, used, uses);
        for (_, term) in arms {
          self.find_used_definitions(term, used, uses);
        }
      }
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::List { .. }
      | Term::Era
      | Term::Invalid => (),
    }
  }

  fn insert_used(&self, def_name: &DefName, used: Used, uses: &mut Definitions) {
    let Entry::Vacant(e) = uses.entry(def_name.clone()) else { return };
    e.insert(used);

    // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
    // E.g.: the `flatten_rules` golden test
    for rule in &self.defs[def_name].rules {
      self.find_used_definitions(&rule.body, used, uses);
    }
  }

  fn insert_ctrs_used(&self, name: &DefName, uses: &mut Definitions) {
    if let Some(Adt { ctrs, .. }) = self.adts.get(name) {
      for (ctr, _) in ctrs {
        self.insert_used(ctr, Used::Adt, uses);
      }
    }
  }
}
