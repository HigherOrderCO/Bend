use std::collections::HashSet;

use crate::{
  diagnostics::Diagnostics,
  fun::{Adts, Constructors, Ctx, Definition, Name, Pattern, Rule, Source, Term},
  maybe_grow,
};

impl Ctx<'_> {
  /// Desugars `fold` expressions into recursive `match`es.
  /// ```bend
  /// foo xs =
  ///   ...
  ///   fold bind = init with x1 x2 {
  ///     Type/Ctr1: (Foo bind.rec_fld bind.fld x1 x2 free_var)
  ///     Type/Ctr2: (Bar bind.fld x1 x2)
  ///   }
  /// ```
  /// Desugars to:
  /// ```bend
  /// foo xs =
  ///   ...
  ///   (foo__fold0 init x1 x2 free_var)
  ///
  /// foo__fold0 = @bind match bind {
  ///   Type/Ctr1: (Foo (foo_fold0 bind.rec_fld x1 x2 free_var) bind.fld x1 x2 free_var)
  ///   Type/Ctr2: (Bar bind.fld x1 x2)
  /// }
  /// ```
  pub fn desugar_fold(&mut self) -> Result<(), Diagnostics> {
    let mut new_defs = vec![];
    for def in self.book.defs.values_mut() {
      let mut fresh = 0;
      for rule in def.rules.iter_mut() {
        let mut ctx = DesugarFoldCtx {
          def_name: &def.name,
          fresh: &mut fresh,
          new_defs: &mut new_defs,
          ctrs: &self.book.ctrs,
          adts: &self.book.adts,
          source: def.source.clone(),
          check: def.check,
        };

        let res = rule.body.desugar_fold(&mut ctx);
        if let Err(e) = res {
          self.info.add_function_error(e, def.name.clone(), def.source.clone());
        }
      }
    }

    self.book.defs.extend(new_defs.into_iter().map(|def| (def.name.clone(), def)));

    self.info.fatal(())
  }
}

struct DesugarFoldCtx<'a> {
  pub def_name: &'a Name,
  pub fresh: &'a mut usize,
  pub new_defs: &'a mut Vec<Definition>,
  pub ctrs: &'a Constructors,
  pub adts: &'a Adts,
  pub source: Source,
  pub check: bool,
}

impl Term {
  fn desugar_fold(&mut self, ctx: &mut DesugarFoldCtx<'_>) -> Result<(), String> {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.desugar_fold(ctx)?;
      }

      if let Term::Fold { .. } = self {
        // Can't have unmatched unscoped because this'll be extracted
        if self.has_unscoped_diff() {
          return Err("Can't have non self-contained unscoped variables in a 'fold'".into());
        }
        let Term::Fold { bnd: _, arg, with_bnd, with_arg, arms } = self else { unreachable!() };

        // Gather the free variables
        let mut free_vars = HashSet::new();
        for arm in arms.iter() {
          let mut arm_free_vars = arm.2.free_vars().into_keys().collect::<HashSet<_>>();
          for field in arm.1.iter().flatten() {
            arm_free_vars.remove(field);
          }
          free_vars.extend(arm_free_vars);
        }
        for var in with_bnd.iter().flatten() {
          free_vars.remove(var);
        }
        let free_vars = free_vars.into_iter().collect::<Vec<_>>();

        let new_nam = Name::new(format!("{}__fold{}", ctx.def_name, ctx.fresh));
        *ctx.fresh += 1;

        // Substitute the implicit recursive calls to call the new function
        let ctr = arms[0].0.as_ref().unwrap();
        let adt_nam = ctx.ctrs.get(ctr).unwrap();
        let ctrs = &ctx.adts.get(adt_nam).unwrap().ctrs;
        for arm in arms.iter_mut() {
          let ctr = arm.0.as_ref().unwrap();
          let recursive = arm
            .1
            .iter()
            .zip(&ctrs.get(ctr).unwrap().fields)
            .filter_map(|(var, field)| if field.rec { Some(var.as_ref().unwrap().clone()) } else { None })
            .collect::<HashSet<_>>();
          arm.2.call_recursive(&new_nam, &recursive, &free_vars);
        }

        // Create the new function
        let x_nam = Name::new("%x");
        let body = Term::Mat {
          arg: Box::new(Term::Var { nam: x_nam.clone() }),
          bnd: None,
          with_bnd: with_bnd.clone(),
          with_arg: with_bnd.iter().map(|nam| Term::var_or_era(nam.clone())).collect(),
          arms: std::mem::take(arms),
        };
        let body = Term::rfold_lams(body, with_bnd.iter().cloned());
        let body = Term::rfold_lams(body, free_vars.iter().map(|nam| Some(nam.clone())));
        let body = Term::lam(Pattern::Var(Some(x_nam)), body);

        let def = Definition::new_gen(
          new_nam.clone(),
          vec![Rule { pats: vec![], body }],
          ctx.source.clone(),
          ctx.check,
        );
        ctx.new_defs.push(def);

        // Call the new function
        let call = Term::call(Term::Ref { nam: new_nam.clone() }, [std::mem::take(arg.as_mut())]);
        let call = Term::call(call, free_vars.iter().cloned().map(|nam| Term::Var { nam }));
        let call = Term::call(call, with_arg.iter().cloned());
        *self = call;
      }
      Ok(())
    })
  }

  fn call_recursive(&mut self, def_name: &Name, recursive: &HashSet<Name>, free_vars: &[Name]) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.call_recursive(def_name, recursive, free_vars);
      }

      // If we found a recursive field, replace with a call to the new function.
      if let Term::Var { nam } = self {
        if recursive.contains(nam) {
          let call = Term::call(Term::Ref { nam: def_name.clone() }, [std::mem::take(self)]);
          let call = Term::call(call, free_vars.iter().cloned().map(|nam| Term::Var { nam }));
          *self = call;
        }
      }
    })
  }
}
