use crate::term::{Book, Name, Pattern, Term};
use hvmc::run::Val;
use std::collections::{hash_map::Entry, HashMap};

impl Book {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_unbound_vars(&self) -> Result<(), String> {
    for (def_name, def) in self.defs.iter() {
      for rule in &def.rules {
        let mut scope = HashMap::new();
        for pat in &rule.pats {
          pat.names().for_each(|nam| push_scope(Some(nam), &mut scope));
        }

        rule.body.check_unbound_vars(&mut scope).map_err(|e| format!("In definition '{def_name}': {e}"))?;
      }
    }
    Ok(())
  }
}

impl Term {
  /// Checks that all variables are bound.
  /// Precondition: References have been resolved, implicit binds have been solved.
  pub fn check_unbound_vars<'a>(&'a self, scope: &mut HashMap<&'a Name, Val>) -> Result<(), String> {
    let mut globals = HashMap::new();
    check_uses(self, scope, &mut globals)?;

    // Check global vars
    for (nam, (declared, used)) in globals {
      match (declared, used) {
        (1, 1) => {}
        (0, _) => return Err(format!("Unbound unscoped variable '${nam}'")),
        (_, 0) => return Err(format!("Unscoped variable from lambda 'λ${nam}' is never used")),
        (1, _) => return Err(format!("Unscoped variable '${nam}' used more than once")),
        (_, 1) => return Err(format!("Unscoped lambda 'λ${nam}' declared more than once")),
        (_, _) => {
          return Err(format!(
            "Unscoped lambda 'λ${nam}' and unscoped variable '${nam}' used more than once"
          ));
        }
      }
    }
    Ok(())
  }
}

/// Scope has the number of times a name was declared in the current scope
/// Globals has how many times a global var name was declared and used.
pub fn check_uses<'a>(
  term: &'a Term,
  scope: &mut HashMap<&'a Name, Val>,
  globals: &mut HashMap<&'a Name, (usize, usize)>,
) -> Result<(), String> {
  // TODO: Don't stop at the first error
  match term {
    Term::Lam { nam, bod, .. } => {
      push_scope(nam.as_ref(), scope);
      check_uses(bod, scope, globals)?;
      pop_scope(nam.as_ref(), scope);
    }
    Term::Var { nam } => {
      if !scope.contains_key(nam) {
        return Err(format!("Unbound variable '{nam}'"));
      }
    }
    Term::Chn { nam, bod, .. } => {
      globals.entry(nam).or_default().0 += 1;
      check_uses(bod, scope, globals)?;
    }
    Term::Lnk { nam } => {
      globals.entry(nam).or_default().1 += 1;
    }
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      check_uses(val, scope, globals)?;
      push_scope(nam.as_ref(), scope);
      check_uses(nxt, scope, globals)?;
      pop_scope(nam.as_ref(), scope);
    }
    Term::Dup { fst, snd, val, nxt, .. }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      check_uses(val, scope, globals)?;
      push_scope(fst.as_ref(), scope);
      push_scope(snd.as_ref(), scope);
      check_uses(nxt, scope, globals)?;
      pop_scope(fst.as_ref(), scope);
      pop_scope(snd.as_ref(), scope);
    }
    Term::Let { .. } => unreachable!(),
    Term::App { fun, arg, .. } => {
      check_uses(fun, scope, globals)?;
      check_uses(arg, scope, globals)?;
    }
    Term::Tup { fst, snd } | Term::Sup { fst, snd, .. } | Term::Opx { fst, snd, .. } => {
      check_uses(fst, scope, globals)?;
      check_uses(snd, scope, globals)?;
    }
    Term::Mat { matched: scrutinee, arms } => {
      check_uses(scrutinee, scope, globals)?;
      for (pat, term) in arms {
        pat.names().for_each(|nam| push_scope(Some(nam), scope));

        check_uses(term, scope, globals)?;

        pat.names().for_each(|nam| pop_scope(Some(nam), scope));
      }
    }
    Term::Lst { .. } => unreachable!(),
    Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era | Term::Err => (),
  }
  Ok(())
}

fn push_scope<'a>(nam: Option<&'a Name>, scope: &mut HashMap<&'a Name, Val>) {
  if let Some(nam) = nam {
    *scope.entry(nam).or_default() += 1;
  }
}

fn pop_scope<'a>(nam: Option<&'a Name>, scope: &mut HashMap<&'a Name, Val>) {
  if let Some(nam) = nam {
    let Entry::Occupied(n_declarations) = scope.entry(nam).and_modify(|e| *e -= 1) else { unreachable!() };

    if *n_declarations.get() == 0 {
      n_declarations.remove();
    }
  }
}
