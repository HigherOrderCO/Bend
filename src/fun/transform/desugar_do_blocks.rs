use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Name, Pattern, Term},
  maybe_grow,
};
use std::collections::HashSet;

impl Ctx<'_> {
  /// Converts `ask` terms inside `do` blocks into calls to a monadic bind operation.
  pub fn desugar_do_blocks(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    let def_names = self.book.defs.keys().cloned().collect::<HashSet<_>>();

    for def in self.book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        if let Err(e) = rule.body.desugar_do_blocks(None, &def_names) {
          self.info.add_rule_error(e, def.name.clone());
        }
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  pub fn desugar_do_blocks(
    &mut self,
    cur_block: Option<&Name>,
    def_names: &HashSet<Name>,
  ) -> Result<(), String> {
    maybe_grow(|| {
      if let Term::Do { typ, bod } = self {
        bod.desugar_do_blocks(Some(typ), def_names)?;
        *self = std::mem::take(bod);
      }

      if let Term::Ask { pat, val, nxt } = self {
        if let Some(typ) = cur_block {
          let fun = make_fun_name(typ);

          if def_names.contains(&fun) {
            let mut fvs = nxt.free_vars();
            pat.binds().flatten().for_each(|bind| _ = fvs.remove(bind));
            let fvs = fvs.into_keys().collect::<Vec<_>>();
            let nxt = fvs
              .iter()
              .fold(*nxt.clone(), |nxt, nam| Term::lam(Pattern::Var(Some(nam.clone())), nxt.clone()));
            let nxt = Term::lam(*pat.clone(), nxt);
            let term = Term::call(Term::Ref { nam: fun.clone() }, [*val.clone(), nxt]);
            *self = Term::call(term, fvs.into_iter().map(|nam| Term::Var { nam }));
          } else {
            return Err(format!("Could not find definition {} for type {}.", fun, typ));
          }
        } else {
          return Err(format!("Monadic bind operation '{pat} <- ...' used outside of a `do` block."));
        }
      }

      for children in self.children_mut() {
        children.desugar_do_blocks(cur_block, def_names)?;
      }

      Ok(())
    })
  }
}

fn make_fun_name(typ: &Name) -> Name {
  Name::new(format!("{typ}/bind"))
}
