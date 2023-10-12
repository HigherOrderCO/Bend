use std::collections::HashMap;

use hvmc::Val;

use crate::term::{DefNames, Name, Term};

/// Checks that all variables are bound and that all globals are used only once
pub fn check_uses(term: &Term, def_names: &DefNames) -> anyhow::Result<()> {
  fn push_scope<'a>(nam: &'a Name, scope: &mut HashMap<&'a Name, Val>) {
    if let Some(n_declarations) = scope.get_mut(nam) {
      *n_declarations += 1;
    } else {
      scope.insert(nam, 1);
    }
  }

  fn pop_scope(nam: &Name, scope: &mut HashMap<&Name, Val>) {
    let n_declarations = scope.get_mut(nam).unwrap();
    *n_declarations -= 1;
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
        go(bod, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::Lam { nam: None, bod } => {
        go(bod, scope, globals, def_names)?;
      }
      Term::Var { nam } => {
        if !scope.contains_key(nam) && !def_names.contains_name(nam) {
          return Err(anyhow::anyhow!("Unbound variable '{nam}'"));
        }
      }
      Term::Chn { nam, bod } => {
        globals.entry(nam).or_default().0 += 1;
        go(bod, scope, globals, def_names)?;
      }
      Term::Lnk { nam } => {
        globals.entry(nam).or_default().1 += 1;
      }
      Term::Let { nam, val, nxt } => {
        go(val, scope, globals, def_names)?;
        push_scope(nam, scope);
        go(nxt, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::App { fun, arg } => {
        go(fun, scope, globals, def_names)?;
        go(arg, scope, globals, def_names)?;
      }
      Term::If { cond, then, els_ } => {
        go(cond, scope, globals, def_names)?;
        go(then, scope, globals, def_names)?;
        go(els_, scope, globals, def_names)?;
      }
      Term::Dup { fst, snd, val, nxt } => {
        go(val, scope, globals, def_names)?;
        if let Some(fst) = fst {
          push_scope(fst, scope)
        }
        if let Some(snd) = snd {
          push_scope(snd, scope);
        }
        go(nxt, scope, globals, def_names)?;
        if let Some(snd) = snd {
          pop_scope(snd, scope);
        }
        if let Some(fst) = fst {
          pop_scope(fst, scope);
        }
      }
      Term::Sup { fst, snd } => {
        go(fst, scope, globals, def_names)?;
        go(snd, scope, globals, def_names)?;
      }
      Term::Ref { .. } | Term::Era => (),
      Term::Opx { fst, snd, .. } => {
        go(fst, scope, globals, def_names)?;
        go(snd, scope, globals, def_names)?;
      }
      Term::Num { .. } => (),
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
