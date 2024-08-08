use crate::{
  fun::{Book, Name, Pattern, Term},
  maybe_grow,
};
use std::collections::HashMap;

impl Book {
  /// Expands the main function so that it is not just a reference.
  /// While technically correct, directly returning a reference is never what users want.
  pub fn expand_main(&mut self) {
    if self.entrypoint.is_none() {
      return;
    }

    let main = self.defs.get_mut(self.entrypoint.as_ref().unwrap()).unwrap();
    let mut main_bod = std::mem::take(&mut main.rule_mut().body);

    let mut seen = vec![self.entrypoint.as_ref().unwrap().clone()];
    main_bod.expand_ref_return(self, &mut seen, &mut 0);

    // Undo the `float_combinators` pass for main, to recover the strictness of the main function.
    main_bod.expand_floated_combinators(self);

    let main = self.defs.get_mut(self.entrypoint.as_ref().unwrap()).unwrap();
    main.rule_mut().body = main_bod;
  }
}

impl Term {
  /// Expands references in the main function that are in "return" position.
  ///
  /// This applies to:
  /// - When main returns a reference.
  /// - When main returns a lambda whose body is a reference.
  /// - When main returns a pair or superposition and one of its elements is a reference.
  ///
  /// Only expand recursive functions once.
  fn expand_ref_return(&mut self, book: &Book, seen: &mut Vec<Name>, globals_count: &mut usize) {
    maybe_grow(|| match self {
      Term::Ref { nam } => {
        if seen.contains(nam) {
          // Don't expand recursive references
        } else if let Some(def) = book.defs.get(nam) {
          // Regular function, expand
          seen.push(nam.clone());
          let mut body = def.rule().body.clone();
          body.rename_unscoped(globals_count, &mut HashMap::new());
          *self = body;
          self.expand_ref_return(book, seen, globals_count);
          seen.pop().unwrap();
        } else {
          // Not a regular function, don't expand
        }
      }
      Term::Fan { els, .. } | Term::List { els } => {
        for el in els {
          el.expand_ref_return(book, seen, globals_count);
        }
      }
      // If an application is just a constructor, we expand the arguments.
      // That way we can write programs like
      // `main = [do_thing1, do_thing2, do_thing3]`
      Term::App { .. } => {
        let (fun, args) = self.multi_arg_app();
        if let Term::Ref { nam } = fun {
          if book.ctrs.contains_key(nam) {
            for arg in args {
              // If the argument is a 0-ary constructor, we don't need to expand it.
              if let Term::Ref { nam } = arg {
                if let Some(adt_nam) = book.ctrs.get(nam) {
                  if book.adts.get(adt_nam).unwrap().ctrs.get(nam).unwrap().fields.is_empty() {
                    continue;
                  }
                }
              }
              // Otherwise, we expand the argument.
              arg.expand_ref_return(book, seen, globals_count);
            }
          }
        }
      }
      Term::Lam { bod: nxt, .. }
      | Term::With { bod: nxt, .. }
      | Term::Open { bod: nxt, .. }
      | Term::Let { nxt, .. }
      | Term::Ask { nxt, .. }
      | Term::Use { nxt, .. } => nxt.expand_ref_return(book, seen, globals_count),
      Term::Var { .. }
      | Term::Link { .. }
      | Term::Num { .. }
      | Term::Nat { .. }
      | Term::Str { .. }
      | Term::Oper { .. }
      | Term::Mat { .. }
      | Term::Swt { .. }
      | Term::Fold { .. }
      | Term::Bend { .. }
      | Term::Def { .. }
      | Term::Era
      | Term::Err => {}
    })
  }

  fn expand_floated_combinators(&mut self, book: &Book) {
    maybe_grow(|| {
      if let Term::Ref { nam } = self {
        if nam.contains(super::float_combinators::NAME_SEP) {
          *self = book.defs.get(nam).unwrap().rule().body.clone();
        }
      }
      for child in self.children_mut() {
        child.expand_floated_combinators(book);
      }
    })
  }

  /// Read the term as an n-ary application.
  fn multi_arg_app(&mut self) -> (&mut Term, Vec<&mut Term>) {
    fn go<'a>(term: &'a mut Term, args: &mut Vec<&'a mut Term>) -> &'a mut Term {
      match term {
        Term::App { fun, arg, .. } => {
          args.push(arg);
          go(fun, args)
        }
        _ => term,
      }
    }
    let mut args = vec![];
    let fun = go(self, &mut args);
    (fun, args)
  }
}

impl Term {
  /// Since expanded functions can contain unscoped variables, and
  /// unscoped variable names must be unique, we need to rename them
  /// to avoid conflicts.
  fn rename_unscoped(&mut self, unscoped_count: &mut usize, unscoped_map: &mut HashMap<Name, Name>) {
    match self {
      Term::Let { pat, .. } | Term::Lam { pat, .. } => pat.rename_unscoped(unscoped_count, unscoped_map),
      Term::Link { nam } => rename_unscoped(nam, unscoped_count, unscoped_map),
      _ => {
        // Isn't an unscoped bind or use, do nothing, just recurse.
      }
    }
    for child in self.children_mut() {
      child.rename_unscoped(unscoped_count, unscoped_map);
    }
  }
}

impl Pattern {
  fn rename_unscoped(&mut self, unscoped_count: &mut usize, unscoped_map: &mut HashMap<Name, Name>) {
    maybe_grow(|| {
      match self {
        Pattern::Chn(nam) => rename_unscoped(nam, unscoped_count, unscoped_map),
        _ => {
          // Pattern isn't an unscoped bind, just recurse.
        }
      }
      for child in self.children_mut() {
        child.rename_unscoped(unscoped_count, unscoped_map);
      }
    })
  }
}

/// Generates a new name for an unscoped variable.
fn rename_unscoped(nam: &mut Name, unscoped_count: &mut usize, unscoped_map: &mut HashMap<Name, Name>) {
  if let Some(new_nam) = unscoped_map.get(nam) {
    *nam = new_nam.clone();
  } else {
    let new_nam = Name::new(format!("{nam}%{}", unscoped_count));
    unscoped_map.insert(nam.clone(), new_nam.clone());
    *unscoped_count += 1;
    *nam = new_nam;
  }
}
