use crate::{
  diagnostics::{Diagnostics, ToStringVerbose},
  maybe_grow,
  term::{Ctx, Name, Pattern, Term},
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug, Clone)]
pub enum UnboundVarErr {
  Local(Name),
  Global { var: Name, declared: usize, used: usize },
}

impl Ctx<'_> {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_unbound_vars(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      let mut errs = Vec::new();
      for rule in &mut def.rules {
        let mut scope = HashMap::new();
        for pat in &rule.pats {
          pat.binds().for_each(|nam| push_scope(nam.as_ref(), &mut scope));
        }

        rule.body.check_unbound_vars(&mut scope, &mut errs);
      }

      for err in errs {
        self.info.add_rule_error(err, def_name.clone());
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
  globals: &mut HashMap<Name, (usize, usize)>,
  errs: &mut Vec<UnboundVarErr>,
) {
  maybe_grow(move || match term {
    Term::Var { nam } => {
      if !scope.contains_key(nam) {
        errs.push(UnboundVarErr::Local(nam.clone()));
        *term = Term::Err;
      }
    }
    Term::Lnk { nam } => {
      globals.entry(nam.clone()).or_default().1 += 1;
    }

    _ => {
      if let Some(pat) = term.pattern() {
        check_global_binds(pat, globals)
      }
      for (child, binds) in term.children_mut_with_binds() {
        for bind in binds.clone() {
          push_scope(bind.as_ref(), scope);
        }
        check_uses(child, scope, globals, errs);
        for bind in binds.rev() {
          pop_scope(bind.as_ref(), scope);
        }
      }
    }
  })
}

pub fn check_global_binds(pat: &Pattern, globals: &mut HashMap<Name, (usize, usize)>) {
  match pat {
    Pattern::Chn(nam) => {
      globals.entry(nam.clone()).or_default().0 += 1;
    }
    _ => {
      for child in pat.children() {
        check_global_binds(child, globals)
      }
    }
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

impl ToStringVerbose for UnboundVarErr {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    match self {
      UnboundVarErr::Local(var) => format!("Unbound variable '{var}'."),
      UnboundVarErr::Global { var, declared, used } => match (declared, used) {
        (0, _) => format!("Unbound unscoped variable '${var}'."),
        (_, 0) => format!("Unscoped variable from lambda 'λ${var}' is never used."),
        (1, _) => format!("Unscoped variable '${var}' used more than once."),
        (_, 1) => format!("Unscoped lambda 'λ${var}' declared more than once."),
        (_, _) => {
          format!("Unscoped lambda 'λ${var}' and unscoped variable '${var}' used more than once.")
        }
      },
    }
  }
}
