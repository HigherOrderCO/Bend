use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, Definition, Name, Rule, Term},
  maybe_grow,
};

const RECURSIVE_KW: &str = "fork";
const NEW_FN_SEP: &str = "__bend";

impl Ctx<'_> {
  pub fn desugar_bend(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();
    let mut new_defs = vec![];
    for def in self.book.defs.values_mut() {
      let mut fresh = 0;
      for rule in def.rules.iter_mut() {
        if let Err(err) = rule.body.desugar_bend(&def.name, &mut fresh, &mut new_defs) {
          self.info.add_rule_error(err, def.name.clone());
          break;
        }
      }
    }

    for def in new_defs {
      self.book.defs.insert(def.name.clone(), def);
    }

    self.info.fatal(())
  }
}

impl Term {
  fn desugar_bend(
    &mut self,
    def_name: &Name,
    fresh: &mut usize,
    new_defs: &mut Vec<Definition>,
  ) -> Result<(), String> {
    maybe_grow(|| {
      // Recursively encode bends in the children
      for child in self.children_mut() {
        child.desugar_bend(def_name, fresh, new_defs)?;
      }

      // Convert a bend into a new recursive function and call it.
      if let Term::Bend { .. } = self {
        // Can't have unmatched unscoped because this'll be extracted
        if self.has_unscoped_diff() {
          return Err("Can't have non self-contained unscoped variables in a 'bend'".into());
        }
        let Term::Bend { bind, init, cond, step, base } = self else { unreachable!() };

        let new_nam = Name::new(format!("{}{}{}", def_name, NEW_FN_SEP, fresh));
        *fresh += 1;

        // Gather the free variables
        let mut free_vars = step.free_vars();
        free_vars.remove(&Name::new(RECURSIVE_KW));
        free_vars.extend(base.free_vars());
        free_vars.extend(cond.free_vars());
        for bind in bind.iter().flatten() {
          free_vars.remove(bind);
        }
        let free_vars = free_vars.into_keys().collect::<Vec<_>>();

        // Substitute the keyword recursive calls to calls to the new function.
        step.fix_bend_call(&new_nam, &free_vars);

        // Create the function for the bend.
        let body = Term::Swt {
          arg: Box::new(std::mem::take(cond)),
          bnd: Some(Name::new("_")),
          with: vec![],
          pred: Some(Name::new("_-1")),
          arms: vec![std::mem::take(base.as_mut()), std::mem::take(step.as_mut())],
        };
        let body = Term::rfold_lams(body, free_vars.iter().cloned().map(Some));
        let body = Term::rfold_lams(body, std::mem::take(bind).into_iter());
        let def =
          Definition { name: new_nam.clone(), rules: vec![Rule { pats: vec![], body }], builtin: false };
        new_defs.push(def);

        // Call the new function in the original term.
        *self = Term::call(Term::Ref { nam: new_nam }, init.drain(..));
        *self = Term::call(std::mem::take(self), free_vars.iter().map(|v| Term::Var { nam: v.clone() }));
      }

      Ok(())
    })
  }

  /// Convert a keyword recursive call to a call to the new function extracted from the bend.
  fn fix_bend_call(&mut self, def_name: &Name, free_vars: &[Name]) {
    maybe_grow(|| {
      if let Term::App { .. } = self {
        // Look for the recursive call
        let mut called = None;
        let mut app = &mut *self;
        while let Term::App { fun, arg, .. } = app {
          arg.fix_bend_call(def_name, free_vars);
          if matches!(fun.as_ref(), Term::App { .. }) {
            app = fun.as_mut();
          } else {
            called = Some(fun.as_mut());
            break;
          }
        }

        // If we found it, replace it with a call to the new function and add the free vars
        if let Some(called) = called
          && let Term::Var { nam } = called
        {
          if nam == RECURSIVE_KW {
            // TODO: Also pass the free vars
            *called = Term::Ref { nam: def_name.clone() };
          }
          *self = Term::call(std::mem::take(self), free_vars.iter().map(|v| Term::Var { nam: v.clone() }));
        }
      } else {
        for child in self.children_mut() {
          child.fix_bend_call(def_name, free_vars);
        }
      }
    })
  }
}
