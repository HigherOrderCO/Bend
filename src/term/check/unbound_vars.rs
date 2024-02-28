use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Pattern, Term},
};
use std::{
  collections::{hash_map::Entry, HashMap},
  fmt::Display,
};

#[derive(Debug, Clone)]
pub enum UnboundVarErr {
  Local(Name),
  Global { var: Name, declared: usize, used: usize },
}

impl Display for UnboundVarErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      UnboundVarErr::Local(var) => write!(f, "Unbound variable '{var}'."),
      UnboundVarErr::Global { var, declared, used } => match (declared, used) {
        (0, _) => write!(f, "Unbound unscoped variable '${var}'."),
        (_, 0) => write!(f, "Unscoped variable from lambda 'λ${var}' is never used."),
        (1, _) => write!(f, "Unscoped variable '${var}' used more than once."),
        (_, 1) => write!(f, "Unscoped lambda 'λ${var}' declared more than once."),
        (_, _) => {
          write!(f, "Unscoped lambda 'λ${var}' and unscoped variable '${var}' used more than once.")
        }
      },
    }
  }
}

impl Ctx<'_> {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_unbound_vars(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      let mut errs = Vec::new();
      for rule in &mut def.rules {
        let mut scope = HashMap::new();
        for pat in &rule.pats {
          pat.binds().for_each(|nam| push_scope(Some(nam), &mut scope));
        }

        rule.body.check_unbound_vars(&mut scope, &mut errs);
      }

      for err in errs {
        self.info.def_error(def_name.clone(), err);
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  /// Checks that all variables are bound.
  /// Precondition: References have been resolved, implicit binds have been solved.

  pub fn check_unbound_vars<'a>(
    &'a mut self,
    scope: &mut HashMap<&'a Name, u64>,
    errs: &mut Vec<UnboundVarErr>,
  ) {
    let mut globals = HashMap::new();
    check_uses(self, scope, &mut globals, errs);

    // Check global vars
    for (nam, (declared, used)) in globals.into_iter().filter(|(_, (d, u))| !(*d == 1 && *u == 1)) {
      errs.push(UnboundVarErr::Global { var: nam.clone(), declared, used });
    }
  }
}

/// Scope has the number of times a name was declared in the current scope
/// Globals has how many times a global var name was declared and used.
pub fn check_uses<'a>(
  term: &'a mut Term,
  scope: &mut HashMap<&'a Name, u64>,
  globals: &mut HashMap<&'a Name, (usize, usize)>,
  errs: &mut Vec<UnboundVarErr>,
) {
  match term {
    Term::Lam { nam, bod, .. } => {
      push_scope(nam.as_ref(), scope);
      check_uses(bod, scope, globals, errs);
      pop_scope(nam.as_ref(), scope);
    }
    Term::Var { nam } => {
      if !scope.contains_key(nam) {
        errs.push(UnboundVarErr::Local(nam.clone()));
        *term = Term::Err;
      }
    }
    Term::Chn { nam, bod, .. } => {
      if let Some(nam) = nam {
        globals.entry(nam).or_default().0 += 1;
      }
      check_uses(bod, scope, globals, errs);
    }
    Term::Lnk { nam } => {
      globals.entry(nam).or_default().1 += 1;
    }
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      check_uses(val, scope, globals, errs);
      push_scope(nam.as_ref(), scope);
      check_uses(nxt, scope, globals, errs);
      pop_scope(nam.as_ref(), scope);
    }
    Term::Dup { bnd, val, nxt, .. } => {
      check_uses(val, scope, globals, errs);
      for bnd in bnd.iter() {
        push_scope(bnd.as_ref(), scope);
      }
      check_uses(nxt, scope, globals, errs);
      for bnd in bnd.iter() {
        pop_scope(bnd.as_ref(), scope);
      }
    }
    Term::Let { pat, val, nxt } => {
      check_uses(val, scope, globals, errs);
      for bnd in pat.bind_or_eras() {
        push_scope(bnd.as_ref(), scope);
      }
      check_uses(nxt, scope, globals, errs);
      for bnd in pat.bind_or_eras() {
        pop_scope(bnd.as_ref(), scope);
      }
    }
    Term::App { fun: fst, arg: snd, .. } | Term::Opx { fst, snd, .. } => {
      check_uses(fst, scope, globals, errs);
      check_uses(snd, scope, globals, errs);
    }
    Term::Mat { args, rules } => {
      for arg in args {
        check_uses(arg, scope, globals, errs);
      }
      for rule in rules {
        rule.pats.iter().flat_map(|p| p.binds()).for_each(|nam| push_scope(Some(nam), scope));

        check_uses(&mut rule.body, scope, globals, errs);

        rule.pats.iter().flat_map(|p| p.binds()).rev().for_each(|nam| pop_scope(Some(nam), scope));
      }
    }
    Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
      for el in els {
        check_uses(el, scope, globals, errs);
      }
    }
    Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era | Term::Err => (),
  }
}

fn push_scope<'a>(nam: Option<&'a Name>, scope: &mut HashMap<&'a Name, u64>) {
  if let Some(nam) = nam {
    *scope.entry(nam).or_default() += 1;
  }
}

fn pop_scope<'a>(nam: Option<&'a Name>, scope: &mut HashMap<&'a Name, u64>) {
  if let Some(nam) = nam {
    let Entry::Occupied(n_declarations) = scope.entry(nam).and_modify(|e| *e -= 1) else { unreachable!() };

    if *n_declarations.get() == 0 {
      n_declarations.remove();
    }
  }
}
