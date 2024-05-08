use core::fmt;
use std::collections::HashSet;

use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Name, Pattern, Term},
  maybe_grow,
};

pub struct MonadicBindError {
  expected_def: Name,
  type_name: Name,
}

impl Ctx<'_> {
  pub fn desugar_do_blocks(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    let def_names = self.book.defs.keys().cloned().collect::<HashSet<_>>();

    for def in self.book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        if let Err(e) = rule.body.desugar_do_blocks(&def_names) {
          self.info.add_rule_error(e, def.name.clone());
        }
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  pub fn desugar_do_blocks(&mut self, def_names: &HashSet<Name>) -> Result<(), MonadicBindError> {
    maybe_grow(|| {
      if let Term::Bind { typ, ask, val, nxt } = self {
        let fun = make_fun_name(typ);

        if def_names.contains(&fun) {
          let mut fvs = nxt.free_vars();
          ask.binds().flatten().for_each(|bind| _ = fvs.remove(bind));
          let fvs = fvs.into_keys().collect::<Vec<_>>();
          let nxt =
            fvs.iter().fold(*nxt.clone(), |nxt, nam| Term::lam(Pattern::Var(Some(nam.clone())), nxt.clone()));
          let nxt = Term::lam(*ask.clone(), nxt);
          let term = Term::call(Term::Ref { nam: fun.clone() }, [*val.clone(), nxt]);
          *self = Term::call(term, fvs.into_iter().map(|nam| Term::Var { nam }));
        } else {
          return Err(MonadicBindError { expected_def: fun, type_name: typ.clone() });
        }
      }

      for children in self.children_mut() {
        children.desugar_do_blocks(def_names)?;
      }

      Ok(())
    })
  }
}

fn make_fun_name(typ: &mut Name) -> Name {
  Name::new(format!("{typ}/bind"))
}

impl fmt::Display for MonadicBindError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Could not find definition {} for type {}.", self.expected_def, self.type_name)
  }
}
