// Pass to give all variables in a definition unique names.

use crate::term::{var_id_to_name, Book, Name, Pattern, Term};
use hvmc::run::Val;
use std::collections::HashMap;

impl Book {
  /// Makes all variables in each definition have a new unique name.
  /// Skips unbound variables.
  /// Precondition: Definition references have been resolved.
  pub fn make_var_names_unique(&mut self) {
    for def in self.defs.values_mut() {
      def.rule_mut().body.make_var_names_unique();
    }
  }
}

impl Term {
  pub fn make_var_names_unique(&mut self) {
    UniqueNameGenerator::default().unique_names_in_term(self);
  }
}

type VarId = Val;

#[derive(Default)]
pub struct UniqueNameGenerator {
  name_map: HashMap<Name, Vec<VarId>>,
  name_count: VarId,
}

impl UniqueNameGenerator {
  // Recursive implementation of unique names pass.
  pub fn unique_names_in_term(&mut self, term: &mut Term) {
    match term {
      // Terms that create names
      Term::Lam { nam, bod, .. } => {
        // Put the name in scope and assign it a unique id.
        // Convert the lambda body and then remove it from scope.
        // Return a lambda with the newly created name
        self.push(nam.as_ref());
        self.unique_names_in_term(bod);
        *nam = self.pop(nam.as_ref());
      }
      Term::Let { pat: Pattern::Var(nam), val, nxt } => {
        self.unique_names_in_term(val);

        self.push(nam.as_ref());
        self.unique_names_in_term(nxt);
        *nam = self.pop(nam.as_ref());
      }
      Term::Dup { tag: _, fst, snd, val, nxt }
      | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
        self.unique_names_in_term(val);

        self.push(fst.as_ref());
        self.push(snd.as_ref());
        self.unique_names_in_term(nxt);
        *snd = self.pop(snd.as_ref());
        *fst = self.pop(fst.as_ref());
      }
      Term::Match { scrutinee, arms } => {
        self.unique_names_in_term(scrutinee);
        for (pat, term) in arms {
          pat.names().for_each(|nam| self.push(Some(nam)));
          self.unique_names_in_term(term);
          pat.names_mut().rev().for_each(|nam| *nam = self.pop(Some(nam)).unwrap());
        }
      }

      // Terms that use names
      Term::Var { nam } => *nam = self.use_var(nam),

      // Others
      Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        self.unique_names_in_term(fst);
        self.unique_names_in_term(snd);
      }
      // Global lam names are already unique, so no need to do anything
      Term::Chn { bod, .. } => self.unique_names_in_term(bod),
      Term::Lnk { .. } | Term::Ref { .. } | Term::Era | Term::Num { .. } | Term::Str { .. } => (),

      Term::Let { .. } => {
        unreachable!("Let terms other than tuple destruction should have been desugared already.")
      }
      Term::List { .. } => unreachable!("Should have been desugared already."),
    }
  }

  fn push(&mut self, nam: Option<&Name>) {
    if let Some(name) = nam {
      self.name_map.entry(name.clone()).or_default().push(self.name_count);
      self.name_count += 1;
    }
  }

  fn pop(&mut self, nam: Option<&Name>) -> Option<Name> {
    if let Some(name) = nam {
      let new_name = self.name_map.get_mut(name).unwrap().pop().unwrap();
      if self.name_map[name].is_empty() {
        self.name_map.remove(name);
      }
      Some(var_id_to_name(new_name))
    } else {
      None
    }
  }

  fn use_var(&self, nam: &Name) -> Name {
    if let Some(vars) = self.name_map.get(nam) {
      let var_id = *vars.last().unwrap();
      var_id_to_name(var_id)
    } else {
      // Skip unbound variables.
      // With this, we can use this function before checking for unbound vars.
      nam.clone()
    }
  }
}
