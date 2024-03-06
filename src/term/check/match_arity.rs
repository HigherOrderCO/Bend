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
    Term::recursive_call(move || {
      if let Term::Mat { args, rules } = self {
        let expected = args.len();
        for rule in rules {
          let found = rule.pats.len();
          if found != expected {
            return Err(MatchErr::ArityMismatch(found, expected));
          }
        }
      }

      for child in self.children() {
        child.check_match_arity()?;
      }
      Ok(())
    })
  }
}
