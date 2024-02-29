use crate::{
  diagnostics::Info,
  term::{transform::encode_pattern_matching::MatchErr, Ctx, Definition, Term},
};

impl Ctx<'_> {
  /// Checks that the number of arguments in every pattern matching rule is consistent.
  pub fn check_match_arity(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter() {
      if let Err(e) = def.check_match_arity() {
        self.info.def_error(def_name.clone(), e)
      };
    }

    self.info.fatal(())
  }
}

impl Definition {
  pub fn check_match_arity(&self) -> Result<(), MatchErr> {
    let expected_arity = self.arity();
    for rule in &self.rules {
      if rule.arity() != expected_arity {
        return Err(MatchErr::ArityMismatch(rule.arity(), expected_arity));
      }
      rule.body.check_match_arity()?;
    }
    Ok(())
  }
}

impl Term {
  pub fn check_match_arity(&self) -> Result<(), MatchErr> {
    stacker::maybe_grow(1024 * 32, 1024 * 1024, move || {
      match self {
        Term::Mat { args, rules } => {
          let expected = args.len();
          for rule in rules {
            let found = rule.pats.len();
            if found != expected {
              return Err(MatchErr::ArityMismatch(found, expected));
            }
            rule.body.check_match_arity()?;
          }
        }

        Term::Lst { els } => {
          for el in els {
            el.check_match_arity()?;
          }
        }
        Term::App { fun: fst, arg: snd, .. }
        | Term::Tup { fst, snd }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Sup { fst, snd, .. }
        | Term::Opx { fst, snd, .. }
        | Term::Let { val: fst, nxt: snd, .. } => {
          fst.check_match_arity()?;
          snd.check_match_arity()?;
        }
        Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.check_match_arity()?,
        Term::Var { .. }
        | Term::Lnk { .. }
        | Term::Num { .. }
        | Term::Str { .. }
        | Term::Ref { .. }
        | Term::Era
        | Term::Err => {}
      }
      Ok(())
    })
  }
}
