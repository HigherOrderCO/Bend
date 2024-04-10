// Pass to give all variables in a definition unique names.

use crate::{
  maybe_grow,
  term::{Book, Name, Term},
};
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
    maybe_grow(|| match term {
      Term::Var { nam } => *nam = self.use_var(nam),
      _ => {
        for (child, binds) in term.children_mut_with_binds_mut() {
          let binds: Vec<_> = binds.collect();
          for bind in binds.iter() {
            self.push(bind.as_ref());
          }
          self.unique_names_in_term(child);
          for bind in binds.into_iter().rev() {
            *bind = self.pop(bind.as_ref());
          }
        }
      }
    })
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
