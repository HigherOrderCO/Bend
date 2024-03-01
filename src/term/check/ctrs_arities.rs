use std::collections::HashMap;

use crate::{
  diagnostics::Info,
  term::{transform::encode_pattern_matching::MatchErr, Book, Ctx, Name, Pattern, Term},
};

impl Ctx<'_> {
  /// Checks if every constructor pattern of every definition rule has the same arity from the
  /// defined adt constructor.
  ///
  /// Constructors should be already resolved.
  pub fn check_ctrs_arities(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    let arities = self.book.ctr_arities();
    for (def_name, def) in self.book.defs.iter() {
      for rule in def.rules.iter() {
        for pat in rule.pats.iter() {
          let res = pat.check_ctrs_arities(&arities);
          self.info.take_err(res, Some(def_name));
        }
        let res = rule.body.check_ctrs_arities(&arities);
        self.info.take_err(res, Some(def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Book {
  /// Returns a hashmap of the constructor name to its arity.
  pub fn ctr_arities(&self) -> HashMap<Name, usize> {
    let mut arities = HashMap::new();

    for adt in self.adts.values() {
      for (ctr_name, fields) in adt.ctrs.iter() {
        arities.insert(ctr_name.clone(), fields.len());
      }
    }

    arities
  }
}

impl Pattern {
  fn check_ctrs_arities(&self, arities: &HashMap<Name, usize>) -> Result<(), MatchErr> {
    let mut to_check = vec![self];

    while let Some(pat) = to_check.pop() {
      match pat {
        Pattern::Ctr(name, args) => {
          let expected = arities.get(name).unwrap();
          let found = args.len();
          if *expected != found {
            return Err(MatchErr::CtrArityMismatch(name.clone(), found, *expected));
          }
        }
        Pattern::Lst(els) | Pattern::Tup(els) => {
          for el in els {
            to_check.push(el);
          }
        }
        Pattern::Var(..) | Pattern::Num(..) | Pattern::Str(_) => {}
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn check_ctrs_arities(&self, arities: &HashMap<Name, usize>) -> Result<(), MatchErr> {
    Term::recursive_call(move || {
      for pat in self.patterns() {
        pat.check_ctrs_arities(arities)?;
      }
      for child in self.children() {
        child.check_ctrs_arities(arities)?;
      }
      Ok(())
    })
  }
}
