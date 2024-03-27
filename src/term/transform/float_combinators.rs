use indexmap::IndexSet;

use crate::term::{Book, Definition, Name, Rule, Term};
use std::collections::BTreeMap;

type Combinators = BTreeMap<Name, Definition>;

impl Book {
  /// Extracts unsafe terms into new definitions.
  ///
  /// Precondition: Variables must have been sanitized.
  ///
  /// The floating algorithm follows these rules:
  /// - Don't extract safe terms or terms that contains unscoped variables.
  /// - Extract if it is a closed Application
  /// - Extract if it is a combinator
  /// - Float every element of a Superposition
  pub fn float_combinators(&mut self) {
    let mut combinators = Combinators::new();

    let slf = self.clone();
    for (def_name, def) in self.defs.iter_mut() {
      let mut name_gen = 0;

      if self.entrypoint.as_ref().is_some_and(|m| m == def_name) {
        continue;
      }

      let builtin = def.builtin;
      let rule = def.rule_mut();
      let mut seen = IndexSet::new();
      rule.body.float_combinators(&mut combinators, &mut name_gen, &slf, def_name, builtin, &mut seen);
    }

    self.defs.extend(combinators);
  }
}

impl Term {
  fn float_combinators(
    &mut self,
    combinators: &mut Combinators,
    name_gen: &mut usize,
    book: &Book,
    def_name: &Name,
    builtin: bool,
    seen: &mut IndexSet<Name>,
  ) {
    Term::recursive_call(move || {
      for term in self.children_mut() {
        // Don't float if it's safe or has unscoped variables.
        if term.is_safe(book, seen) || term.has_unscoped_diff() {
          continue;
        }

        // Recursively float the children terms.
        term.float_combinators(combinators, name_gen, book, def_name, builtin, seen);

        match term {
          // If it is an Application without free variables like '(bar 0)', float into a new definition.
          Term::App { .. } => {
            if term.free_vars().is_empty() && !term.has_unscoped_diff() {
              float_combinator(def_name, name_gen, term, builtin, combinators);
            }
          }

          // If it is a Superposition, float every child element.
          Term::Sup { els, .. } => els
            .iter_mut()
            .for_each(|e| e.float_combinators(combinators, name_gen, book, def_name, builtin, seen)),

          // If it is a combinator, float into a new definition.
          term if term.is_combinator() => float_combinator(def_name, name_gen, term, builtin, combinators),

          _ => continue,
        }
      }
    })
  }
}

/// Inserts a new definition for the given term in the combinators map.
fn float_combinator(
  def_name: &Name,
  name_gen: &mut usize,
  term: &mut Term,
  builtin: bool,
  combinators: &mut BTreeMap<Name, Definition>,
) {
  let comb_name = Name::new(format!("{}$C{}", def_name, *name_gen));
  *name_gen += 1;

  let comb_var = Term::Ref { nam: comb_name.clone() };
  let extracted_term = std::mem::replace(term, comb_var);

  let rules = vec![Rule { body: extracted_term, pats: Vec::new() }];
  let rule = Definition { name: comb_name.clone(), rules, builtin };
  combinators.insert(comb_name, rule);
}

impl Term {
  /// A term can be considered safe if it is:
  /// - A Number or an Eraser.
  /// - A Tuple or Superposition where all elements are safe.
  /// - A constant Lambda, e.g. a nullary constructor.
  /// - A Reference with safe body.
  pub fn is_safe(&self, book: &Book, seen: &mut IndexSet<Name>) -> bool {
    Term::recursive_call(move || match self {
      Term::Num { .. } | Term::Era => true,

      Term::Tup { els } | Term::Sup { els, .. } => els.iter().all(|e| Term::is_safe(e, book, seen)),

      Term::Lam { .. } => self.is_constant_lambda(book, seen),

      Term::Ref { nam } => {
        if seen.contains(nam) {
          return false;
        }

        seen.insert(nam.clone());
        if let Some(definition) = book.defs.get(nam) {
          definition.rule().body.is_safe(book, seen)
        } else {
          false
        }
      }

      _ => false,
    })
  }

  /// Checks if the term is a lambda sequence with the body being a variable in the scope or a reference.
  fn is_constant_lambda(&self, book: &Book, seen: &mut IndexSet<Name>) -> bool {
    let mut current = self;
    let mut scope = Vec::new();

    while let Term::Lam { nam, bod, .. } = current {
      if let Some(nam) = nam {
        scope.push(nam);
      }
      current = bod;
    }

    match current {
      Term::Var { nam } if scope.contains(&nam) => true,
      Term::Ref { .. } => true,
      term => term.is_safe(book, seen),
    }
  }

  /// A term is a combinator if it is a lambda abstraction without free variables.
  pub fn is_combinator(&self) -> bool {
    matches!(self, Term::Lam { .. } if self.free_vars().is_empty())
  }

  pub fn has_unscoped_diff(&self) -> bool {
    let (declared, used) = self.unscoped_vars();
    declared.difference(&used).count() != 0
  }
}
