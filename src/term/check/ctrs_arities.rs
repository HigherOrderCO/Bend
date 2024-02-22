use std::{collections::HashMap, fmt::Display};

use crate::{
  diagnostics::Info,
  term::{Book, Ctx, Name, Pattern},
};

#[derive(Debug, Clone)]
pub struct ArityErr(Name, usize, usize);

impl Display for ArityErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Arity error. Constructor '{}' expects {} fields, found {}.", self.0, self.1, self.2)
  }
}

impl<'book> Ctx<'book> {
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
          let res = pat.check(&arities);
          self.info.take_err(res, Some(&def_name));
        }
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
  fn check(&self, arities: &HashMap<Name, usize>) -> Result<(), ArityErr> {
    let mut to_check = vec![self];

    while let Some(pat) = to_check.pop() {
      match pat {
        Pattern::Ctr(name, args) => {
          let arity = arities.get(name).unwrap();
          let args = args.len();
          if *arity != args {
            return Err(ArityErr(name.clone(), *arity, args));
          }
        }
        Pattern::Tup(fst, snd) => {
          to_check.push(fst);
          to_check.push(snd);
        }
        Pattern::Lst(els) => {
          for el in els {
            to_check.push(el);
          }
        }
        Pattern::Var(..) | Pattern::Num(..) => {}
      }
    }
    Ok(())
  }
}
