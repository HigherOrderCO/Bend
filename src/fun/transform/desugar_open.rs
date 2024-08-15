use crate::{
  diagnostics::Diagnostics,
  fun::{Adts, Ctx, Term},
  maybe_grow,
};

impl Ctx<'_> {
  pub fn desugar_open(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for def in self.book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        if let Err(err) = rule.body.desugar_open(&self.book.adts) {
          self.info.add_function_error(err, def.name.clone(), def.source.clone());
        }
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  fn desugar_open(&mut self, adts: &Adts) -> Result<(), String> {
    maybe_grow(|| {
      match self {
        Term::Open { typ, var, bod } => {
          bod.desugar_open(adts)?;
          if let Some(adt) = adts.get(&*typ) {
            if adt.ctrs.len() == 1 {
              let ctr = adt.ctrs.keys().next().unwrap();
              *self = Term::Mat {
                arg: Box::new(Term::Var { nam: var.clone() }),
                bnd: Some(std::mem::take(var)),
                with_bnd: vec![],
                with_arg: vec![],
                arms: vec![(Some(ctr.clone()), vec![], std::mem::take(bod))],
              }
            } else {
              return Err(format!("Type '{typ}' of an 'open' has more than one constructor"));
            }
          } else {
            return Err(format!("Type '{typ}' of an 'open' is not defined"));
          }
        }
        Term::Def { def, nxt } => {
          for rule in def.rules.iter_mut() {
            rule.body.desugar_open(adts)?;
          }
          nxt.desugar_open(adts)?;
        }
        _ => {
          for child in self.children_mut() {
            child.desugar_open(adts)?;
          }
        }
      }
      Ok(())
    })
  }
}
