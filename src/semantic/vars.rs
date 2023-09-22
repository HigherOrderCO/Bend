use hvm_core::Val;

use crate::ast::{DefId, DefinitionBook, Name, Term};
use std::collections::{HashMap, HashSet};

/// For all var declarations:
///   If they're used 0 times: erase the declaration
///   If they're used 1 time: leave them as-is
///   If they're used more times: insert dups to make var use affine
/// For all let vars:
///   If they're used 0 times: why? discard the let
///   If they're used 1 time: substitute the body in the var use
///   If they're use more times: add dups for all the uses, put the body at the root dup.
/// For all definition references: Convert from a Var term to an actual Ref term.
/// Reports any unbound variables.
/// Precondition: The pattern matching rules and constructors have already been converted into lambda calculus.

impl DefinitionBook {
  pub fn sanitize_vars(&mut self) {
    for (def_id, def) in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        check_uses(&rule.body, def_names)
      }
    }
    todo!()
  }
}

type VarId = DefId;

/// Checks that all variables are bound and that all globals are used only once
fn check_uses<'a>(term: &'a Term, def_names: &HashSet<&'a Name>) -> anyhow::Result<()> {
  fn push_scope<'a>(nam: &'a Name, scope: &mut HashMap<&'a Name, Val>) {
    if let Some(n_declarations) = scope.get_mut(nam) {
      *n_declarations += 1;
    } else {
      scope.insert(nam, 1);
    }
  }

  fn pop_scope<'a>(nam: &'a Name, scope: &mut HashMap<&'a Name, Val>) {
    let n_declarations = scope.get_mut(nam).unwrap();
    *n_declarations -= 0;
    if *n_declarations == 0 {
      scope.remove(nam);
    }
  }

  /// Scope has the number of times a name was declared in the current scope
  /// Globals has how many times a global var name was declared and used.
  fn go<'a>(
    term: &'a Term,
    scope: &mut HashMap<&'a Name, Val>,
    globals: &mut HashMap<&'a Name, (Val, Val)>,
    def_names: &HashSet<&'a Name>,
  ) -> anyhow::Result<()> {
    // TODO: Don't stop at the first error
    match term {
      Term::Lam { nam: Some(nam), bod } => {
        push_scope(nam, scope);
        go(&bod, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::Lam { nam: None, bod } => {
        go(&bod, scope, globals, def_names)?;
      }
      Term::Var { nam } => {
        if !scope.contains_key(nam) && !def_names.contains(nam) {
          return Err(anyhow::anyhow!("Unbound variable {nam}"));
        }
      }
      Term::GlobalLam { nam, bod } => {
        globals.entry(nam).or_default().0 += 1;
        go(&bod, scope, globals, def_names)?;
      }
      Term::GlobalVar { nam } => {
        globals.entry(nam).or_default().1 += 1;
      }
      Term::Let { nam, val, nxt } => {
        go(&val, scope, globals, def_names)?;
        push_scope(nam, scope);
        go(&nxt, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::App { fun, arg } => {
        go(&fun, scope, globals, def_names)?;
        go(&arg, scope, globals, def_names)?;
      }
      Term::Dup { fst, snd, val, nxt } => {
        go(&val, scope, globals, def_names)?;
        if let Some(fst) = fst {
          push_scope(fst, scope)
        }
        if let Some(snd) = snd {
          push_scope(snd, scope);
        }
        go(&nxt, scope, globals, def_names)?;
        if let Some(snd) = snd {
          pop_scope(snd, scope);
        }
        if let Some(fst) = fst {
          pop_scope(fst, scope);
        }
      }
      Term::NumOp { .. } | Term::Ref { .. } | Term::Num { .. } => (),
      Term::Sup { .. } | Term::Era => unreachable!(),
    }
    Ok(())
  }

  let mut globals = HashMap::new();
  go(term, &mut HashMap::new(), &mut globals, def_names)?;

  // Check global vars
  for (nam, (n_decls, n_uses)) in globals.into_iter() {
    if n_decls == 0 {
      return Err(anyhow::anyhow!("Global variable ${nam} declared more than once."));
    } else if n_uses == 0 {
      // TODO: Convert into local erased lambda
      return Err(anyhow::anyhow!("Global variable ${nam} was not used."));
    } else if n_uses > 1 {
      // TODO: Add dups for global vars
      return Err(anyhow::anyhow!("Global variable ${nam} used more than once."));
    }
  }
  Ok(())
}

type UniqueNameScope = HashMap<Name, Vec<VarId>>;

fn unique_var_names(
  term: &Term,
  name_map: &mut UniqueNameScope,
  name_count: &mut VarId,
) -> anyhow::Result<Term> {
  fn push_name(name: Name, name_map: &mut UniqueNameScope, name_count: &mut VarId) {
    name_map.entry(name).or_default().push(*name_count);
    name_count.0 += 1;
  }

  fn pop_name(name: &Name, name_map: &mut UniqueNameScope) -> VarId {
    let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
    if name_map[name].is_empty() {
      name_map.remove(name);
    }
    new_name
  }

  let term = match term {
    Term::Lam { nam: Some(nam), bod } => {
      // Put the name in scope and assign it a unique id.
      // Convert the lambda body and then remove it from scope.
      // Return a lambda with the newly created name
      push_name(nam.clone(), name_map, name_count);
      let bod = unique_var_names(bod, name_map, name_count)?;
      let nam = pop_name(&nam, name_map);
      Term::Lam { nam: Some(Name::from(nam)), bod: Box::new(bod) }
    }
    Term::Lam { nam: None, bod } => {
      Term::Lam { nam: None, bod: Box::new(unique_var_names(bod, name_map, name_count)?) }
    }
    Term::Var { nam } => {
      if let Some(vars) = name_map.get(nam) {
        let nam = *vars.last().unwrap();
        Term::Var { nam: Name::from(nam) }
      } else {
        // If it's not in scope, we know it must be a Ref by our preconditions
        Term::Ref { def_id: DefId::from(nam) }
      }
    }
    Term::GlobalLam { nam, bod } => {
      // Global lam names are already unique, so no need to do anything
      let bod = unique_var_names(bod, name_map, name_count)?;
      Term::GlobalLam { nam: nam.clone(), bod: Box::new(bod) }
    }
    Term::Let { nam, val, nxt } => todo!(),
    Term::Ref { def_id } => todo!(),
    Term::App { fun, arg } => todo!(),
    Term::Dup { fst, snd, val, nxt } => todo!(),
    Term::Num { val } => todo!(),
    Term::NumOp { op, fst, snd } => todo!(),
    Term::Sup { fst, snd } => todo!(),
    t => t.clone(),
  };
  Ok(term)
}
