use crate::{
  diagnostics::Diagnostics,
  fun::{transform::desugar_bend, Ctx, Name, Pattern, Term},
  maybe_grow,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum UnboundVarErr {
  Local(Name),
  Global { var: Name, declared: usize, used: usize },
}

impl Ctx<'_> {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_unbound_vars(&mut self) -> Result<(), Diagnostics> {
    for (def_name, def) in self.book.defs.iter_mut() {
      let mut errs = Vec::new();
      for rule in &mut def.rules {
        // Note: Using a Vec instead of a Map is a deliberate optimization.
        let mut scope = rule.pats.iter().flat_map(|pat| pat.binds()).map(|x| x.as_ref()).collect::<Vec<_>>();
        rule.body.check_unbound_vars(&mut scope, &mut errs);
      }

      for err in errs {
        self.info.add_function_error(err, def_name.clone(), def.source.clone());
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
    scope: &mut Vec<Option<&'a Name>>,
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
  scope: &mut Vec<Option<&'a Name>>,
  globals: &mut HashMap<Name, (usize, usize)>,
  errs: &mut Vec<UnboundVarErr>,
) {
  maybe_grow(move || match term {
    Term::Var { nam } => {
      if !scope_contains(nam, scope) {
        errs.push(UnboundVarErr::Local(nam.clone()));
        *term = Term::Err;
      }
    }
    Term::Link { nam } => {
      globals.entry(nam.clone()).or_default().1 += 1;
    }

    _ => {
      if let Some(pat) = term.pattern() {
        check_global_binds(pat, globals)
      }
      for (child, binds) in term.children_mut_with_binds() {
        for bind in binds.clone() {
          scope.push(bind.as_ref());
        }
        check_uses(child, scope, globals, errs);
        for _ in binds {
          scope.pop();
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

fn scope_contains(nam: &Name, scope: &[Option<&Name>]) -> bool {
  scope.iter().rev().any(|scope_nam| scope_nam == nam)
}

impl std::fmt::Display for UnboundVarErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      UnboundVarErr::Local(var) => {
        if var == desugar_bend::RECURSIVE_KW {
          write!(
            f,
            "Unbound variable '{}'.\n    Note: '{}' is only a keyword inside the 'when' arm of a 'bend'.",
            var,
            desugar_bend::RECURSIVE_KW
          )
        } else if let Some((pre, suf)) = var.rsplit_once('-') {
          write!(
            f,
            "Unbound variable '{var}'. If you wanted to subtract '{pre}' from '{suf}', you must separate it with spaces ('{pre} - {suf}') since '-' is a valid name character."
          )
        } else {
          write!(f, "Unbound variable '{var}'.")
        }
      }
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
