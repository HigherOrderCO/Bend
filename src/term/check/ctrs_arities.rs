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
        Pattern::Err => unreachable!(),
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn check_ctrs_arities(&self, arities: &HashMap<Name, usize>) -> Result<(), MatchErr> {
    match self {
      Term::Mat { args, rules } => {
        for arg in args {
          arg.check_ctrs_arities(arities)?;
        }
        for rule in rules {
          for pat in &rule.pats {
            pat.check_ctrs_arities(arities)?;
          }
          rule.body.check_ctrs_arities(arities)?;
        }
      }
      Term::Let { pat, val, nxt } => {
        pat.check_ctrs_arities(arities)?;
        val.check_ctrs_arities(arities)?;
        nxt.check_ctrs_arities(arities)?;
      }

      Term::Lst { els } => {
        for el in els {
          el.check_ctrs_arities(arities)?;
        }
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.check_ctrs_arities(arities)?;
        snd.check_ctrs_arities(arities)?;
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.check_ctrs_arities(arities)?,
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
    Ok(())
  }
}
