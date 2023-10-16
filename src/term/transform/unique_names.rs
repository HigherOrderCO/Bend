// Pass to give all variables in a definition unique names.

use crate::term::{var_id_to_name, DefinitionBook, Name, Term};
use hvmc::run::Val;
use std::collections::HashMap;

impl DefinitionBook {
  /// Makes all variables in each definition have a new unique name.
  /// Precondition: Definition references have been resolved, no unbound variables.
  pub fn make_var_names_unique(&mut self) {
    for def in self.defs.values_mut() {
      def.body.make_var_names_unique()
    }
  }
}

impl Term {
  pub fn make_var_names_unique(&mut self) {
    let new_term = unique_var_names(self, &mut Default::default(), &mut 0);
    *self = new_term;
  }
}

type VarId = Val;
type UniqueNameScope = HashMap<Name, Vec<VarId>>;

// Recursive implementation of unique names pass.
fn unique_var_names(term: &Term, name_map: &mut UniqueNameScope, name_count: &mut VarId) -> Term {
  match term {
    Term::Lam { nam: Some(nam), bod } => {
      // Put the name in scope and assign it a unique id.
      // Convert the lambda body and then remove it from scope.
      // Return a lambda with the newly created name
      push_name(nam.clone(), name_map, name_count);
      let bod = unique_var_names(bod, name_map, name_count);
      let nam = pop_name(nam, name_map);
      Term::Lam { nam: Some(nam), bod: Box::new(bod) }
    }
    Term::Lam { nam: None, bod } => {
      let bod = unique_var_names(bod, name_map, name_count);
      Term::Lam { nam: None, bod: Box::new(bod) }
    }
    Term::Var { nam } => {
      let nam = use_var(nam, name_map).unwrap();
      Term::Var { nam }
    }
    Term::Chn { nam, bod } => {
      // Global lam names are already unique, so no need to do anything
      let bod = unique_var_names(bod, name_map, name_count);
      Term::Chn { nam: nam.clone(), bod: Box::new(bod) }
    }
    Term::Let { nam, val, nxt } => {
      let val = unique_var_names(val, name_map, name_count);
      push_name(nam.clone(), name_map, name_count);
      let nxt = unique_var_names(nxt, name_map, name_count);
      let nam = pop_name(nam, name_map);
      Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) }
    }
    Term::App { fun, arg } => {
      let fun = unique_var_names(fun, name_map, name_count);
      let arg = unique_var_names(arg, name_map, name_count);
      Term::App { fun: Box::new(fun), arg: Box::new(arg) }
    }
    Term::Dup { fst, snd, val, nxt } => {
      let val = unique_var_names(val, name_map, name_count);
      if let Some(fst) = fst {
        push_name(fst.clone(), name_map, name_count);
      }
      if let Some(snd) = snd {
        push_name(snd.clone(), name_map, name_count);
      }
      let nxt = unique_var_names(nxt, name_map, name_count);
      let snd = snd.as_ref().map(|snd| pop_name(snd, name_map));
      let fst = fst.as_ref().map(|fst| pop_name(fst, name_map));
      Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
    }
    Term::Sup { fst, snd } => {
      let fst = unique_var_names(fst, name_map, name_count);
      let snd = unique_var_names(snd, name_map, name_count);
      Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }
    }
    Term::Opx { op, fst, snd } => {
      let fst = unique_var_names(fst, name_map, name_count);
      let snd = unique_var_names(snd, name_map, name_count);
      Term::Opx { op: *op, fst: Box::new(fst), snd: Box::new(snd) }
    }
    Term::Match { cond, zero, succ } => {
      let cond = unique_var_names(cond, name_map, name_count);
      let zero = unique_var_names(zero, name_map, name_count);
      let succ = unique_var_names(succ, name_map, name_count);
      Term::Match { cond: Box::new(cond), zero: Box::new(zero), succ: Box::new(succ) }
    }
    t @ (Term::Lnk { .. } | Term::Ref { .. } | Term::Era | Term::Num { .. }) => t.clone(),
  }
}

fn push_name(name: Name, name_map: &mut UniqueNameScope, name_count: &mut VarId) {
  name_map.entry(name).or_default().push(*name_count);
  *name_count += 1;
}

fn pop_name(name: &Name, name_map: &mut UniqueNameScope) -> Name {
  let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
  if name_map[name].is_empty() {
    name_map.remove(name);
  }
  var_id_to_name(new_name)
}

fn use_var(nam: &Name, name_map: &UniqueNameScope) -> Option<Name> {
  if let Some(vars) = name_map.get(nam) {
    let var_id = *vars.last().unwrap();
    let new_name = var_id_to_name(var_id);
    Some(new_name)
  } else {
    None
  }
}
