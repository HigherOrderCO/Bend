use std::collections::{hash_map::Entry, HashMap};

use crate::{
  term::{Adt, AdtEncoding, Book, DefName, Origin, Tag, Term, LCONS, LNIL, SCONS, SNIL},
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
  pub fn prune(
    &mut self,
    main: Option<&DefName>,
    prune_all: bool,
    adt_encoding: AdtEncoding,
    warnings: &mut Vec<Warning>,
  ) {
    let mut used = Definitions::new();

    if let Some(main) = main {
      let def = self.defs.get(main).unwrap();
      used.insert(main.clone(), Used::Main);
      self.find_used_definitions(&def.rule().body, Used::Main, &mut used, adt_encoding);
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

            self.find_used_definitions(&rule.body, Used::Unused, &mut used, adt_encoding);
          }
        }
      }
    }

    // Filter defs from the 'used' hashmap that are not accessible from main
    let filter = |(name, used)| if used == Used::Unused { None } else { Some(name) };
    let used: IndexSet<DefName> = used.into_iter().filter_map(filter).collect();

    let names = self.defs.keys().cloned().collect::<IndexSet<DefName>>();
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
  fn find_used_definitions(
    &self,
    term: &Term,
    used: Used,
    uses: &mut Definitions,
    adt_encoding: AdtEncoding,
  ) {
    self.find_manual_adt_encoding(term, uses, adt_encoding);

    match term {
      Term::Ref { def_name } => match self.ctrs.get(def_name) {
        Some(name) => self.insert_ctrs_used(name, uses, adt_encoding),
        None => self.insert_used(def_name, used, uses, adt_encoding),
      },

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        self.find_used_definitions(bod, used, uses, adt_encoding)
      }
      Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        self.find_used_definitions(fst, used, uses, adt_encoding);
        self.find_used_definitions(snd, used, uses, adt_encoding);
      }
      Term::Match { scrutinee, arms } => {
        self.find_used_definitions(scrutinee, used, uses, adt_encoding);
        for (_, term) in arms {
          self.find_used_definitions(term, used, uses, adt_encoding);
        }
      }
      Term::List { els } => {
        self.insert_ctrs_used(&DefName::new(LCONS), uses, adt_encoding);
        self.insert_ctrs_used(&DefName::new(LNIL), uses, adt_encoding);
        for term in els {
          self.find_used_definitions(term, used, uses, adt_encoding);
        }
      }
      Term::Str { .. } => {
        self.insert_ctrs_used(&DefName::new(SCONS), uses, adt_encoding);
        self.insert_ctrs_used(&DefName::new(SNIL), uses, adt_encoding);
      }
      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Era | Term::Invalid => (),
    }
  }

  /// If the current term is a manual encoding of a constructor or adt, we marked it as used.
  fn find_manual_adt_encoding(&self, term: &Term, uses: &mut Definitions, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => (),
      // If using the tagged scott encoding of ADTs, we also mark a constructor
      // as used if the user manually encoded a ctr or match (or any other use of the ctr tags).
      AdtEncoding::TaggedScott => match term {
        Term::Lam { tag: Tag::Named(name), .. }
        | Term::Chn { tag: Tag::Named(name), .. }
        | Term::App { tag: Tag::Named(name), .. } => {
          // We don't check dup/sup tags because they use a separate label scope.
          self.insert_ctrs_used(name, uses, adt_encoding);
        }
        _ => (),
      },
    }
  }

  fn insert_used(&self, def_name: &DefName, used: Used, uses: &mut Definitions, adt_encoding: AdtEncoding) {
    if let Entry::Vacant(e) = uses.entry(def_name.clone()) {
      e.insert(used);

      // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
      // E.g.: the `flatten_rules` golden test
      for rule in &self.defs[def_name].rules {
        self.find_used_definitions(&rule.body, used, uses, adt_encoding);
      }
    }
  }

  fn insert_ctrs_used(&self, name: &DefName, uses: &mut Definitions, adt_encoding: AdtEncoding) {
    if let Some(Adt { ctrs, .. }) = self.adts.get(name) {
      for (ctr, _) in ctrs {
        self.insert_used(ctr, Used::Adt, uses, adt_encoding);
      }
    }
  }
}
