use crate::{
  builtins::{CORE_BUILTINS, CORE_BUILTINS_USES},
  diagnostics::{ToStringVerbose, WarningType},
  term::{Adt, AdtEncoding, Book, Ctx, Name, Tag, Term, LIST, STRING},
};
use indexmap::IndexSet;
use std::collections::{hash_map::Entry, HashMap};

struct UnusedDefinitionWarning;

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

type Definitions = HashMap<Name, Used>;

impl Ctx<'_> {
  /// If `prune_all`, removes all unused definitions and adts starting from Main.
  /// Otherwise, prunes only the builtins not accessible from any non-built-in definition
  pub fn prune(&mut self, prune_all: bool, adt_encoding: AdtEncoding) {
    let mut used = Definitions::new();

    if let Some(main) = &self.book.entrypoint {
      let def = self.book.defs.get(main).unwrap();
      used.insert(main.clone(), Used::Main);
      self.book.find_used_definitions(&def.rule().body, Used::Main, &mut used, adt_encoding);
    }

    if let AdtEncoding::Scott = adt_encoding {
      for (def_name, def) in &self.book.defs {
        if !def.builtin && self.book.ctrs.get(def_name).is_some() {
          used.insert(def_name.clone(), Used::Adt);
        }
      }
    }

    // Even if we don't prune all the defs, we need check what built-ins are accessible through user code
    if !prune_all {
      for (def_name, def) in &self.book.defs {
        // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
        // E.g.: the `flatten_rules` golden test
        if !def.builtin {
          for rule in &def.rules {
            match used.entry(def_name.clone()) {
              Entry::Vacant(e) => _ = e.insert(Used::Unused),
              Entry::Occupied(e) if *e.get() != Used::Unused => continue,
              _ => {}
            }

            self.book.find_used_definitions(&rule.body, Used::Unused, &mut used, adt_encoding);
          }
        }
      }
    }

    // Filter defs from the 'used' hashmap that are not accessible from main
    let filter = |(name, used)| if used == Used::Unused { None } else { Some(name) };
    let used: IndexSet<Name> = used.into_iter().filter_map(filter).collect();

    let names = self.book.defs.keys().cloned().collect::<IndexSet<Name>>();
    let unused = names.difference(&used).cloned();

    self.prune_unused(unused, prune_all);
  }

  fn prune_unused(&mut self, unused: impl IntoIterator<Item = Name>, prune_all: bool) {
    for def_name in unused {
      let def = &self.book.defs[&def_name];
      if prune_all || def.builtin {
        self.book.defs.shift_remove(&def_name);
      } else if !def_name.is_generated() {
        self.info.add_rule_warning(UnusedDefinitionWarning, WarningType::UnusedDefinition, def_name);
      }
    }
  }
}

impl Book {
  /// Finds all used definitions on every term that can have a def_id.
  fn find_used_definitions(
    &self,
    term: &Term,
    used: Used,
    uses: &mut Definitions,
    adt_encoding: AdtEncoding,
  ) {
    let mut to_find = vec![term];

    while let Some(term) = to_find.pop() {
      self.find_manual_adt_encoding(term, uses, adt_encoding);

      match term {
        Term::Ref { nam: def_name } => match self.ctrs.get(def_name) {
          Some(name) => self.insert_ctrs_used(name, uses, adt_encoding),
          None => self.insert_used(def_name, used, uses, adt_encoding),
        },
        Term::Lst { .. } => {
          self.insert_ctrs_used(&Name::from(LIST), uses, adt_encoding);
        }
        Term::Str { .. } => {
          self.insert_ctrs_used(&Name::from(STRING), uses, adt_encoding);
        }
        _ => {}
      }

      for child in term.children() {
        to_find.push(child);
      }
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

  fn insert_used(&self, def_name: &Name, used: Used, uses: &mut Definitions, adt_encoding: AdtEncoding) {
    if let Entry::Vacant(e) = uses.entry(def_name.clone()) {
      e.insert(used);
      if let Some(position) = CORE_BUILTINS.iter().position(|x| x == &def_name.0.as_ref()) {
        for def_name in CORE_BUILTINS_USES[position] {
          self.insert_used(&Name::new(*def_name), used, uses, adt_encoding);
        }
        return;
      }

      // This needs to be done for each rule in case the pass it's ran from has not encoded the pattern match
      // E.g.: the `flatten_rules` golden test
      for rule in &self.defs[def_name].rules {
        self.find_used_definitions(&rule.body, used, uses, adt_encoding);
      }
    }
  }

  fn insert_ctrs_used(&self, name: &Name, uses: &mut Definitions, adt_encoding: AdtEncoding) {
    if let Some(Adt { ctrs, .. }) = self.adts.get(name) {
      for (ctr, _) in ctrs {
        self.insert_used(ctr, Used::Adt, uses, adt_encoding);
      }
    }
  }
}

impl ToStringVerbose for UnusedDefinitionWarning {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    "Definition is unused.".to_string()
  }
}
