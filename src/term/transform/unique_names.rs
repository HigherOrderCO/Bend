// Pass to give all variables in a definition unique names.

use crate::term::{Book, Name, Pattern, Term};
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

type VarId = u64;

#[derive(Default)]
pub struct UniqueNameGenerator {
  name_map: HashMap<Name, Vec<VarId>>,
  name_count: VarId,
}

impl UniqueNameGenerator {
  // Recursively assign an id to each variable in the term, then convert each id into a unique name.
  pub fn unique_names_in_term(&mut self, term: &mut Term) {
    match term {
      // Terms that create names
      Term::Lam { nam, bod, .. } => {
        // Put the name in scope and assign it a unique id.
        // Convert the lambda body and then remove it from scope.
        // Return a lambda with the newly created name
        {
          self.push(nam.as_ref());
          self.unique_names_in_term(bod);
        }
        *nam = self.pop(nam.as_ref());
      }
      Term::Let { pat: Pattern::Var(nam), val, nxt } => {
        self.unique_names_in_term(val);

        self.push(nam.as_ref());
        self.unique_names_in_term(nxt);
        *nam = self.pop(nam.as_ref());
      }
      Term::Dup { tag: _, bnd, val, nxt } => {
        self.unique_names_in_term(val);

        for bnd in bnd.iter() {
          self.push(bnd.as_ref());
        }
        self.unique_names_in_term(nxt);
        for bnd in bnd.iter_mut().rev() {
          *bnd = self.pop(bnd.as_ref());
        }
      }
      Term::Let { pat, val, nxt } => {
        self.unique_names_in_term(val);

        for bnd in pat.bind_or_eras() {
          self.push(bnd.as_ref());
        }
        self.unique_names_in_term(nxt);
        for bnd in pat.bind_or_eras_mut().rev() {
          *bnd = self.pop(bnd.as_ref());
        }
      }
      Term::Mat { args, rules } => {
        for arg in args {
          self.unique_names_in_term(arg);
        }
        for rule in rules {
          rule.pats.iter().flat_map(|p| p.bind_or_eras().flatten()).for_each(|nam| self.push(Some(nam)));
          self.unique_names_in_term(&mut rule.body);
          rule
            .pats
            .iter_mut()
            .flat_map(|p| p.bind_or_eras_mut().flatten())
            .rev()
            .for_each(|nam| *nam = self.pop(Some(nam)).unwrap());
        }
      }

      // Terms that use names
      Term::Var { nam } => *nam = self.use_var(nam),

      // Others
      Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
        for el in els {
          self.unique_names_in_term(el);
        }
      }
      Term::App { fun: fst, arg: snd, .. } | Term::Opx { fst, snd, .. } => {
        self.unique_names_in_term(fst);
        self.unique_names_in_term(snd);
      }
      // Global lam names are already unique, so no need to do anything
      Term::Chn { bod, .. } => self.unique_names_in_term(bod),
      Term::Lnk { .. } | Term::Ref { .. } | Term::Era | Term::Num { .. } | Term::Str { .. } | Term::Err => (),
    }
  }

  fn push(&mut self, nam: Option<&Name>) {
    if let Some(name) = nam {
      if let Some(ids) = self.name_map.get_mut(name) {
        ids.push(self.name_count);
      } else {
        self.name_map.insert(name.clone(), vec![self.name_count]);
      }
      self.name_count += 1;
    }
  }

  fn pop(&mut self, nam: Option<&Name>) -> Option<Name> {
    if let Some(name) = nam {
      let var_id = self.name_map.get_mut(name).unwrap().pop().unwrap();
      if self.name_map[name].is_empty() {
        self.name_map.remove(name);
      }
      Some(Name::from(var_id))
    } else {
      None
    }
  }

  fn use_var(&self, nam: &Name) -> Name {
    if let Some(vars) = self.name_map.get(nam) {
      let var_id = *vars.last().unwrap();
      Name::from(var_id)
    } else {
      // Skip unbound variables.
      // With this, we can use this function before checking for unbound vars.
      nam.clone()
    }
  }
}
