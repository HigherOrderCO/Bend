use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Name, Term},
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
      if let Term::With { typ, bod } = self {
        bod.desugar_do_blocks(Some(typ), def_names)?;
        let wrap_ref = Term::r#ref(&format!("{typ}/wrap"));
        // let wrap_ref = if def_names.contains(&wrap_nam) {
        //   Term::r#ref(&wrap_nam)
        // } else {
        //   return Err(format!("Could not find definition {wrap_nam} for type {typ}"));
        // };
        *self = Term::Use { nam: Some(Name::new("wrap")), val: Box::new(wrap_ref), nxt: std::mem::take(bod) };
      }

      if let Term::Ask { pat, val, nxt } = self {
        if let Some(typ) = cur_block {
          let bind_nam = Name::new(format!("{typ}/bind"));

          if def_names.contains(&bind_nam) {
            // TODO: come up with a strategy for forwarding free vars to prevent infinite recursion.
            let nxt = Term::lam(*pat.clone(), std::mem::take(nxt));
            *self = Term::call(Term::Ref { nam: bind_nam }, [*val.clone(), nxt]);
          } else {
            return Err(format!("Could not find definition {bind_nam} for type {typ}."));
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
