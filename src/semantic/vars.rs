use hvm_core::Val;

use crate::ast::{hvm_lang::DefNames, var_id_to_name, DefId, DefinitionBook, Name, Term};
use std::collections::HashMap;

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
  pub fn sanitize_vars(&mut self) -> anyhow::Result<()> {
    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        check_uses(&rule.body, &self.def_names)?;
        let (body, mut var_uses) = unique_var_names(&rule.body, &self.def_names)?;
        let body = term_to_affine(body, &mut var_uses, &mut HashMap::new())?;
        rule.body = body;
      }
    }
    todo!()
  }
}

/// Checks that all variables are bound and that all globals are used only once
fn check_uses<'a>(term: &'a Term, def_names: &DefNames) -> anyhow::Result<()> {
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
    def_names: &DefNames,
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
        if !scope.contains_key(nam) && !def_names.contains_right(nam) {
          return Err(anyhow::anyhow!("Unbound variable {nam}"));
        }
      }
      Term::Chn { nam, bod } => {
        globals.entry(nam).or_default().0 += 1;
        go(&bod, scope, globals, def_names)?;
      }
      Term::Lnk { nam } => {
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
      Term::Opx { fst, snd, .. } => {
        go(&fst, scope, globals, def_names)?;
        go(&snd, scope, globals, def_names)?;
      }
      Term::Sup { fst, snd } => {
        go(&fst, scope, globals, def_names)?;
        go(&snd, scope, globals, def_names)?;
      }
      Term::Ref { .. } | Term::U32 { .. } | Term::I32 { .. } | Term::Era => (),
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

type VarId = Val;
type UniqueNameScope = HashMap<Name, Vec<VarId>>;

/// Gives every declared variable a unique name and converts refs into Ref terms.
/// Also returns how many times each variable (with its new unique name) is used.
fn unique_var_names(term: &Term, def_names: &DefNames) -> anyhow::Result<(Term, HashMap<Name, Val>)> {
  fn push_name(name: Name, name_map: &mut UniqueNameScope, name_count: &mut VarId) {
    name_map.entry(name).or_default().push(*name_count);
    *name_count += 1;
  }

  fn pop_name(name: &Name, name_map: &mut UniqueNameScope) -> VarId {
    let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
    if name_map[name].is_empty() {
      name_map.remove(name);
    }
    new_name
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
        push_name(nam.clone(), name_map, name_count);
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        let var_id = pop_name(&nam, name_map);
        Term::Lam { nam: Some(var_id_to_name(var_id)), bod: Box::new(bod) }
      }
      Term::Lam { nam: None, bod } => {
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Lam { nam: None, bod: Box::new(bod) }
      }
      Term::Var { nam } => {
        if let Some(vars) = name_map.get(nam) {
          let var_id = *vars.last().unwrap();
          Term::Var { nam: var_id_to_name(var_id) }
        } else {
          // If it's not in scope, we know it must be a Ref by our preconditions
          Term::Ref { def_id: *def_names.get_by_right(nam).unwrap() }
        }
      }
      Term::Chn { nam, bod } => {
        // Global lam names are already unique, so no need to do anything
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Chn { nam: nam.clone(), bod: Box::new(bod) }
      }
      Term::Let { nam, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        push_name(nam.clone(), name_map, name_count);
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let var_id = pop_name(&nam, name_map);
        Term::Let { nam: var_id_to_name(var_id), val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::App { fun, arg } => {
        let fun = go(fun, name_map, name_count, var_uses, def_names)?;
        let arg = go(arg, name_map, name_count, var_uses, def_names)?;
        Term::App { fun: Box::new(fun), arg: Box::new(arg) }
      }
      Term::Dup { fst, snd, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        if let Some(fst) = fst {
          push_name(fst.clone(), name_map, name_count);
        }
        if let Some(snd) = snd {
          push_name(snd.clone(), name_map, name_count);
        }
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let snd = if let Some(snd) = snd {
          let var_id = pop_name(&snd, name_map);
          Some(var_id_to_name(var_id))
        } else {
          None
        };
        let fst = if let Some(fst) = fst {
          let var_id = pop_name(&fst, name_map);
          Some(var_id_to_name(var_id))
        } else {
          None
        };
        Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::Opx { op, fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Opx { op: *op, fst: Box::new(fst), snd: Box::new(snd) }
      }
      Term::Sup { fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }
      }
      t => t.clone(),
    };
    Ok(term)
  }

  let mut var_uses = HashMap::new();
  let term = go(term, &mut Default::default(), &mut 0, &mut var_uses, def_names)?;
  Ok((term, var_uses))
}

/// Erases variables that weren't used, dups the ones that were used more than once.
/// Substitutes lets into their variable use.
fn term_to_affine(
  term: Term,
  var_uses: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) -> anyhow::Result<Term> {
  fn duplicate_lam(nam: Name, mut nxt: Term, uses: Val) -> (Option<Name>, Term) {
    // TODO: Is there a difference between a list of dups and a complete binary tree of dups?
    let nxt = match uses {
      0 | 1 => nxt,
      uses => {
        nxt = Term::Dup {
          fst: Some(dup_name(&nam, uses)),
          snd: Some(dup_name(&nam, uses - 1)),
          val: Box::new(Term::Var { nam: nam.clone() }),
          nxt: Box::new(nxt),
        };
        for i in uses - 2 .. 0 {
          nxt = Term::Dup {
            fst: Some(dup_name(&nam, i)),
            snd: Some(Name(format!("{}_dup", dup_name(&nam, i)))),
            val: Box::new(Term::Var { nam: nam.clone() }),
            nxt: Box::new(nxt),
          }
        }
        nxt
      }
    };
    let nam = match uses {
      0 => None,
      1 => Some(dup_name(&nam, 1)),
      _ => Some(nam),
    };
    (nam, nxt)
  }

  fn duplicate_let(nam: Name, mut nxt: Term, let_body: Term, uses: Val) -> Term {
    nxt = Term::Dup {
      fst: Some(dup_name(&nam, uses)),
      snd: Some(dup_name(&nam, uses - 1)),
      val: Box::new(let_body),
      nxt: Box::new(nxt),
    };
    for i in uses - 2 .. 0 {
      nxt = Term::Dup {
        fst: Some(dup_name(&nam, i)),
        snd: Some(Name(format!("{}_dup", dup_name(&nam, i)))),
        val: Box::new(Term::Var { nam: nam.clone() }),
        nxt: Box::new(nxt),
      }
    }
    nxt
  }

  fn dup_name(nam: &Name, uses: Val) -> Name {
    Name(format!("{nam}_{uses}"))
  }

  let term = match term {
    Term::Lam { nam: None, bod } => {
      Term::Lam { nam: None, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) }
    }

    Term::Lam { nam: Some(nam), bod } => {
      let uses = var_uses[&nam];
      let bod = term_to_affine(*bod, var_uses, let_bodies)?;
      let (nam, bod) = duplicate_lam(nam, bod, uses);
      Term::Lam { nam, bod: Box::new(bod) }
    }
    Term::Var { nam } => {
      let uses = var_uses[&nam];
      *var_uses.get_mut(&nam).unwrap() -= 1;
      if let Some(subst) = let_bodies.remove(&nam) { subst } else { Term::Var { nam: dup_name(&nam, uses) } }
    }
    Term::Chn { nam, bod } => Term::Chn { nam, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) },
    Term::Let { nam, val, nxt } => {
      let uses = var_uses[&nam];
      match uses {
        0 => term_to_affine(*nxt, var_uses, let_bodies)?,
        1 => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let_bodies.insert(nam, val);
          term_to_affine(*nxt, var_uses, let_bodies)?
        }
        uses => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
          duplicate_let(nam, nxt, val, uses)
        }
      }
    }
    Term::Dup { fst, snd, val, nxt } => {
      let uses_fst = fst.as_ref().map(|fst| *var_uses.get(fst).unwrap()).unwrap_or(0);
      let uses_snd = snd.as_ref().map(|snd| *var_uses.get(snd).unwrap()).unwrap_or(0);

      let val = term_to_affine(*val, var_uses, let_bodies)?;
      let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
      let (fst, nxt) = if let Some(fst) = fst { duplicate_lam(fst, nxt, uses_fst) } else { (fst, nxt) };
      let (snd, nxt) = if let Some(snd) = snd { duplicate_lam(snd, nxt, uses_snd) } else { (snd, nxt) };
      Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
    }
    Term::App { fun, arg } => Term::App {
      fun: Box::new(term_to_affine(*fun, var_uses, let_bodies)?),
      arg: Box::new(term_to_affine(*arg, var_uses, let_bodies)?),
    },
    Term::Opx { op, fst, snd } => Term::Opx {
      op,
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    Term::Sup { fst, snd } => Term::Sup {
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    t => t,
  };
  Ok(term)
}
