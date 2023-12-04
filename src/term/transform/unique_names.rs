// Pass to give all variables in a definition unique names.

use crate::term::{var_id_to_name, Book, MatchNum, Name, Pattern, Term};
use hvmc::run::Val;
use std::collections::HashMap;

impl Book {
  /// Makes all variables in each definition have a new unique name.
  /// Precondition: Definition references have been resolved, no unbound variables.
  pub fn make_var_names_unique(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.make_var_names_unique()
      }
    }
  }
}

impl Term {
  pub fn make_var_names_unique(&mut self) {
    unique_var_names(self, &mut Default::default(), &mut 0);
  }
}

type VarId = Val;
type UniqueNameScope = HashMap<Name, Vec<VarId>>;

// Recursive implementation of unique names pass.
fn unique_var_names(term: &mut Term, name_map: &mut UniqueNameScope, name_count: &mut VarId) {
  match term {
    Term::Lam { nam, bod, .. } => {
      // Put the name in scope and assign it a unique id.
      // Convert the lambda body and then remove it from scope.
      // Return a lambda with the newly created name
      push_name(nam.as_ref(), name_map, name_count);
      unique_var_names(bod, name_map, name_count);
      *nam = pop_name(nam.as_ref(), name_map);
    }
    Term::Var { nam } => *nam = use_var(nam, name_map),
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      unique_var_names(val, name_map, name_count);
      push_name(nam.as_ref(), name_map, name_count);
      unique_var_names(nxt, name_map, name_count);
      *nam = pop_name(nam.as_ref(), name_map);
    }
    Term::Dup { tag: _, fst, snd, val, nxt }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      unique_var_names(val, name_map, name_count);
      push_name(fst.as_ref(), name_map, name_count);
      push_name(snd.as_ref(), name_map, name_count);
      unique_var_names(nxt, name_map, name_count);
      *snd = pop_name(snd.as_ref(), name_map);
      *fst = pop_name(fst.as_ref(), name_map);
    }
    Term::Let { .. } => unreachable!(),
    // Global lam names are already unique, so no need to do anything
    Term::Chn { bod, .. } => unique_var_names(bod, name_map, name_count),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      unique_var_names(fst, name_map, name_count);
      unique_var_names(snd, name_map, name_count);
    }
    Term::Match { scrutinee, arms } => {
      unique_var_names(scrutinee, name_map, name_count);
      for (rule, term) in arms {
        if let Pattern::Num(MatchNum::Succ(nam)) = rule {
          push_name(nam.as_ref(), name_map, name_count)
        }

        unique_var_names(term, name_map, name_count);

        if let Pattern::Num(MatchNum::Succ(nam)) = rule {
          *nam = pop_name(nam.as_ref(), name_map)
        }
      }
    }
    Term::Lnk { .. } | Term::Ref { .. } | Term::Era | Term::Num { .. } => (),
  }
}

fn push_name(name: Option<&Name>, name_map: &mut UniqueNameScope, name_count: &mut VarId) {
  if let Some(name) = name {
    name_map.entry(name.clone()).or_default().push(*name_count);
    *name_count += 1;
  }
}

fn pop_name(name: Option<&Name>, name_map: &mut UniqueNameScope) -> Option<Name> {
  if let Some(name) = name {
    let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
    if name_map[name].is_empty() {
      name_map.remove(name);
    }
    Some(var_id_to_name(new_name))
  } else {
    None
  }
}

fn use_var(nam: &Name, name_map: &UniqueNameScope) -> Name {
  let vars = name_map.get(nam).unwrap();
  let var_id = *vars.last().unwrap();
  var_id_to_name(var_id)
}
