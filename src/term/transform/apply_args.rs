use std::fmt::Display;

use crate::{
  diagnostics::Info,
  term::{Ctx, Pattern, Term},
};

#[derive(Clone, Debug)]
pub enum ArgError {
  PatternArgError,
  ArityArgError { expected: usize, got: usize },
}

impl Display for ArgError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ArgError::PatternArgError => write!(f, ""),
      ArgError::ArityArgError { expected, got } => write!(f, "Expected {expected} arguments, got {got}."),
    }
  }
}

impl Ctx<'_> {
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), Info> {
    self.info.start_pass();

    if let Some(entrypoint) = &self.book.entrypoint
      && let Some(args) = args
    {
      let main_def = &mut self.book.defs[entrypoint];
      let expected = main_def.rules[0].pats.len();
      let got = args.len();

      if !main_def.rules[0].pats.iter().all(|pat| matches!(pat, Pattern::Var(Some(..)))) {
        self.info.def_error(entrypoint.clone(), ArgError::PatternArgError);
      }

      if expected != got {
        self.info.error(ArgError::ArityArgError { expected, got });
      }

      main_def.convert_match_def_to_term();
      let main_body = &mut self.book.defs[entrypoint].rule_mut().body;

      *main_body = Term::call(main_body.clone(), args);
    }

    self.info.fatal(())
  }
}
