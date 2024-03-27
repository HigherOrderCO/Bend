use crate::term::{Book, Definition, Name, Rule, Term};
use std::collections::BTreeMap;

type Combinators = BTreeMap<Name, Definition>;

impl Book {
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
      rule.body.float_combinators(&mut combinators, &mut name_gen, &slf, def_name, builtin);
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
  ) {
    Term::recursive_call(move || {
      for term in self.children_mut() {
        if term.is_constant() || term.is_safe(book) || term.has_unscoped_diff() {
          continue;
        }

        term.float_combinators(combinators, name_gen, book, def_name, builtin);

        match term {
          Term::App { .. } => {
            if term.free_vars().is_empty() && !term.has_unscoped_diff() {
              float_combinator(def_name, name_gen, term, builtin, combinators);
            }
          }

          Term::Sup { els, .. } => {
            els.iter_mut().for_each(|e| e.float_combinators(combinators, name_gen, book, def_name, builtin))
          }

          term if term.is_combinator() => float_combinator(def_name, name_gen, term, builtin, combinators),

          _ => term.float_combinators(combinators, name_gen, book, def_name, builtin),
        }
      }
    })
  }
}

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
  pub fn is_safe(&self, book: &Book) -> bool {
    Term::recursive_call(move || match self {
      // A number or an eraser is safe.
      Term::Num { .. } | Term::Era => true,

      // A tuple or superposition with only constant elements is safe.
      Term::Tup { els } | Term::Sup { els, .. } => els.iter().all(Term::is_constant),

      // A constant lambda sequence is safe.
      Term::Lam { bod: box Term::Var { .. }, .. } => self.is_constant_lambda(),

      // A lambda with a safe body is safe.
      Term::Lam { bod, .. } => bod.is_safe(book),

      // A ref to a function with a safe body is safe if the function is in the book and its body is safe.
      Term::Ref { nam } => {
        if let Some(definition) = book.defs.get(nam) {
          definition.rule().body.is_safe(book)
        } else {
          false
        }
      }

      // TODO?: Any term that, when fully expanded, becomes a supercombinator is safe.
      // _ => self.is_supercombinator(),
      _ => false,
    })
  }

  pub fn is_constant(&self) -> bool {
    match self {
      Term::Num { .. } => true,
      Term::Era => true,
      Term::Tup { els } | Term::Sup { els, .. } => els.iter().all(Term::is_constant),
      Term::Lam { .. } => self.is_constant_lambda(),
      _ => false,
    }
  }

  /// Checks if the term is a lambda sequence with the body being a variable in the scope or a reference.
  fn is_constant_lambda(&self) -> bool {
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
      other => other.is_constant(),
    }
  }

  fn is_combinator(&self) -> bool {
    matches!(self, Term::Lam { .. } if self.free_vars().is_empty())
  }

  fn has_unscoped_diff(&self) -> bool {
    let (declared, used) = self.unscoped_vars();
    declared.difference(&used).count() != 0
  }
}
