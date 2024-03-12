use crate::{
  diagnostics::{Diagnostics, ToStringVerbose},
  term::{Ctx, Definition, Term},
};

pub struct MatchArityMismatchErr {
  expected: usize,
  found: usize,
}

impl Ctx<'_> {
  /// Checks that the number of arguments in every pattern matching rule is consistent.
  pub fn check_match_arity(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter() {
      let res = def.check_match_arity();
      self.info.take_rule_err(res, def_name.clone());
    }

    self.info.fatal(())
  }
}

impl Definition {
  pub fn check_match_arity(&self) -> Result<(), MatchArityMismatchErr> {
    let expected = self.arity();
    for rule in &self.rules {
      let found = rule.arity();
      if found != expected {
        return Err(MatchArityMismatchErr { found, expected });
      }
      rule.body.check_match_arity()?;
    }
    Ok(())
  }
}

impl Term {
  pub fn check_match_arity(&self) -> Result<(), MatchArityMismatchErr> {
    Term::recursive_call(move || {
      if let Term::Mat { args, rules } = self {
        let expected = args.len();
        for rule in rules {
          let found = rule.pats.len();
          if found != expected {
            return Err(MatchArityMismatchErr { found, expected });
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

impl ToStringVerbose for MatchArityMismatchErr {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    format!("Arity mismatch in pattern matching. Expected {} patterns, found {}.", self.expected, self.found)
  }
}
