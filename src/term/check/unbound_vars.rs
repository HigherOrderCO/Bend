use crate::term::{Book, MatchNum, Name, Pattern, Term};
use hvmc::run::Val;
use std::collections::HashMap;

impl Book {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_unbound_vars(&self) -> Result<(), String> {
    for (def_id, def) in &self.defs {
      def.assert_no_pattern_matching_rules();
      let def_name = self.def_names.name(def_id).unwrap();
      def.rules[0].body.check_unbound_vars(def_name)?;
    }
    Ok(())
  }
}

impl Term {
  /// Checks that all variables are bound.
  /// Precondition: References have been resolved.
  pub fn check_unbound_vars(&self, def_name: &Name) -> Result<(), String> {
    let mut globals = HashMap::new();
    check_uses(self, &mut HashMap::new(), &mut globals, def_name)?;

    // Check global vars
    for (nam, (declared, used)) in globals.into_iter() {
      if used && !declared {
        return Err(format!("Unbound unscoped variable '${nam}' in definition '{def_name}'"));
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
  globals: &mut HashMap<&'a Name, (bool, bool)>,
  def_name: &Name,
) -> Result<(), String> {
  // TODO: Don't stop at the first error
  match term {
    Term::Lam { nam, bod, .. } => {
      push_scope(nam.as_ref(), scope);
      check_uses(bod, scope, globals, def_name)?;
      pop_scope(nam.as_ref(), scope);
    }
    Term::Var { nam } => {
      if !scope.contains_key(nam) {
        return Err(format!("Unbound variable '{nam}' in definition '{def_name}'"));
      }
    }
    Term::Chn { nam, bod, .. } => {
      globals.entry(nam).or_default().0 = true;
      check_uses(bod, scope, globals, def_name)?;
    }
    Term::Lnk { nam } => {
      globals.entry(nam).or_default().1 = true;
    }
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      check_uses(val, scope, globals, def_name)?;
      push_scope(nam.as_ref(), scope);
      check_uses(nxt, scope, globals, def_name)?;
      pop_scope(nam.as_ref(), scope);
    }
    Term::Dup { fst, snd, val, nxt, .. }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      check_uses(val, scope, globals, def_name)?;
      push_scope(fst.as_ref(), scope);
      push_scope(snd.as_ref(), scope);
      check_uses(nxt, scope, globals, def_name)?;
      pop_scope(fst.as_ref(), scope);
      pop_scope(snd.as_ref(), scope);
    }
    Term::Let { .. } => unreachable!(),
    Term::App { fun, arg, .. } => {
      check_uses(fun, scope, globals, def_name)?;
      check_uses(arg, scope, globals, def_name)?;
    }
    Term::Tup { fst, snd } | Term::Sup { fst, snd, .. } | Term::Opx { fst, snd, .. } => {
      check_uses(fst, scope, globals, def_name)?;
      check_uses(snd, scope, globals, def_name)?;
    }
    Term::Match { scrutinee, arms } => {
      check_uses(scrutinee, scope, globals, def_name)?;
      for (pat, term) in arms {
        if let Pattern::Num(MatchNum::Succ(Some(nam))) = pat {
          push_scope(nam.as_ref(), scope);
        }

        check_uses(term, scope, globals, def_name)?;

        if let Pattern::Num(MatchNum::Succ(Some(nam))) = pat {
          pop_scope(nam.as_ref(), scope);
        }
      }
    }
    Term::List { .. } => unreachable!(),
    Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => (),
  }
  Ok(())
}

fn push_scope<'a>(nam: Option<&'a Name>, scope: &mut HashMap<&'a Name, Val>) {
  if let Some(nam) = nam {
    if let Some(n_declarations) = scope.get_mut(nam) {
      *n_declarations += 1;
    } else {
      scope.insert(nam, 1);
    }
  }
}

fn pop_scope(nam: Option<&Name>, scope: &mut HashMap<&Name, Val>) {
  if let Some(nam) = nam {
    let n_declarations = scope.get_mut(nam).unwrap();
    *n_declarations -= 1;
    if *n_declarations == 0 {
      scope.remove(nam);
    }
  }
}
