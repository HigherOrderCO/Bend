// Pass to give all variables in a definition unique names

use crate::term::{var_id_to_name, DefNames, Name, Term};
use hvmc::Val;
use std::collections::HashMap;

type VarId = Val;
type UniqueNameScope = HashMap<Name, Vec<VarId>>;

/// Gives every declared variable a unique name and converts refs into Ref terms.
/// Also returns how many times each variable (with its new unique name) is used.
/// Precondition: No unbound variables in the term.
pub fn unique_var_names(term: &Term, def_names: &DefNames) -> anyhow::Result<(Term, HashMap<Name, Val>)> {
  fn push_name(
    name: Name,
    name_map: &mut UniqueNameScope,
    name_count: &mut VarId,
    var_uses: &mut HashMap<Name, Val>,
  ) {
    name_map.entry(name).or_default().push(*name_count);
    var_uses.insert(var_id_to_name(*name_count), 0);
    *name_count += 1;
  }

  fn pop_name(name: &Name, name_map: &mut UniqueNameScope) -> Name {
    let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
    if name_map[name].is_empty() {
      name_map.remove(name);
    }
    var_id_to_name(new_name)
  }

  fn use_var(nam: &Name, name_map: &UniqueNameScope, var_uses: &mut HashMap<Name, Val>) -> Option<Name> {
    if let Some(vars) = name_map.get(nam) {
      let var_id = *vars.last().unwrap();
      let new_name = var_id_to_name(var_id);
      *var_uses.get_mut(&new_name).unwrap() += 1;
      Some(new_name)
    } else {
      None
    }
  }

  fn go(
    term: &Term,
    name_map: &mut UniqueNameScope,
    name_count: &mut VarId,
    var_uses: &mut HashMap<Name, Val>,
    def_names: &DefNames,
  ) -> anyhow::Result<Term> {
    let term = match term {
      Term::Lam { nam: Some(nam), bod } => {
        // Put the name in scope and assign it a unique id.
        // Convert the lambda body and then remove it from scope.
        // Return a lambda with the newly created name
        push_name(nam.clone(), name_map, name_count, var_uses);
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        let nam = pop_name(nam, name_map);
        Term::Lam { nam: Some(nam), bod: Box::new(bod) }
      }
      Term::Lam { nam: None, bod } => {
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Lam { nam: None, bod: Box::new(bod) }
      }
      Term::Var { nam } => {
        if let Some(nam) = use_var(nam, name_map, var_uses) {
          Term::Var { nam }
        } else {
          // If it's not in scope, we know it must be a Ref by our preconditions
          Term::Ref { def_id: def_names.def_id(nam).unwrap() }
        }
      }
      Term::Chn { nam, bod } => {
        // Global lam names are already unique, so no need to do anything
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Chn { nam: nam.clone(), bod: Box::new(bod) }
      }
      Term::Let { nam, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        push_name(nam.clone(), name_map, name_count, var_uses);
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let nam = pop_name(nam, name_map);
        Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::App { fun, arg } => {
        let fun = go(fun, name_map, name_count, var_uses, def_names)?;
        let arg = go(arg, name_map, name_count, var_uses, def_names)?;
        Term::App { fun: Box::new(fun), arg: Box::new(arg) }
      }
      Term::Dup { fst, snd, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        if let Some(fst) = fst {
          push_name(fst.clone(), name_map, name_count, var_uses);
        }
        if let Some(snd) = snd {
          push_name(snd.clone(), name_map, name_count, var_uses);
        }
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let snd = snd.as_ref().map(|snd| pop_name(snd, name_map));
        let fst = fst.as_ref().map(|fst| pop_name(fst, name_map));
        Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::Sup { fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }
      }
      Term::Opx { op, fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Opx { op: *op, fst: Box::new(fst), snd: Box::new(snd) }
      }
      Term::If { cond, then, els_ } => {
        let cond = go(cond, name_map, name_count, var_uses, def_names)?;
        let then = go(then, name_map, name_count, var_uses, def_names)?;
        let els_ = go(els_, name_map, name_count, var_uses, def_names)?;
        Term::If { cond: Box::new(cond), then: Box::new(then), els_: Box::new(els_) }
      }
      t @ (Term::Lnk { .. } | Term::Ref { .. } | Term::Era | Term::Num { .. }) => t.clone(),
    };
    Ok(term)
  }

  let mut var_uses = HashMap::new();
  let term = go(term, &mut Default::default(), &mut 0, &mut var_uses, def_names)?;
  Ok((term, var_uses))
}
