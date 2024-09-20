use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Name, Pattern, Term},
  maybe_grow,
};
use std::collections::HashSet;

impl Ctx<'_> {
  /// Converts `ask` terms inside `with` blocks into calls to a monadic bind operation.
  pub fn desugar_with_blocks(&mut self) -> Result<(), Diagnostics> {
    let def_names = self.book.defs.keys().cloned().collect::<HashSet<_>>();

    for def in self.book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        if let Err(e) = rule.body.desugar_with_blocks(None, &def_names) {
          self.info.add_function_error(e, def.name.clone(), def.source.clone());
        }
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  pub fn desugar_with_blocks(
    &mut self,
    cur_block: Option<&Name>,
    def_names: &HashSet<Name>,
  ) -> Result<(), String> {
    maybe_grow(|| {
      if let Term::With { typ, bod } = self {
        bod.desugar_with_blocks(Some(typ), def_names)?;
        let wrap_ref = Term::r#ref(&format!("{typ}/wrap"));
        *self = Term::Use { nam: Some(Name::new("wrap")), val: Box::new(wrap_ref), nxt: std::mem::take(bod) };
      }

      if let Term::Ask { pat, val, nxt } = self {
        if let Some(typ) = cur_block {
          let bind_nam = Name::new(format!("{typ}/bind"));

          if def_names.contains(&bind_nam) {
            let nxt = Term::lam(*pat.clone(), std::mem::take(nxt));
            let nxt = nxt.defer();

            *self = Term::call(Term::Ref { nam: bind_nam }, [*val.clone(), nxt]);
          } else {
            return Err(format!("Could not find definition {bind_nam} for type {typ}."));
          }
        } else {
          return Err(format!("Monadic bind operation '{pat} <- ...' used outside of a `do` block."));
        }
      }

      for children in self.children_mut() {
        children.desugar_with_blocks(cur_block, def_names)?;
      }

      Ok(())
    })
  }

  /// Converts a term with free vars `(f x1 .. xn)` into a deferred
  /// call that passes those vars to the term.
  ///
  /// Ex: `(f x1 .. xn)` becomes `@x (x @x1 .. @xn (f x1 .. x2) x1 .. x2)`.
  ///
  /// The user must call this lazy thunk by calling the builtin
  /// `undefer` function, or by applying `@x x` to the term.
  fn defer(self) -> Term {
    let free_vars = self.free_vars().into_keys().collect::<Vec<_>>();
    let term = Term::rfold_lams(self, free_vars.iter().cloned().map(Some));
    let term = Term::call(Term::Var { nam: Name::new("%x") }, [term]);
    let term = Term::call(term, free_vars.iter().cloned().map(|nam| Term::Var { nam }));
    Term::lam(Pattern::Var(Some(Name::new("%x"))), term)
  }
}
