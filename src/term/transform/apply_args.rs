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
  /// Applies the arguments to the program being run by applying them to the main function.
  ///
  /// Example:
  /// ```hvm
  /// main x1 x2 x3 = (MainBody x1 x2 x3)
  /// ```
  /// Calling with `hvml run <file> arg1 arg2 arg3`, it becomes:
  /// ```hvm
  /// main = (λx1 λx2 λx3 (MainBody x1 x2 x3) arg1 arg2 arg3)
  /// ```
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), Info> {
    self.info.start_pass();

    if let Some(entrypoint) = &self.book.entrypoint {
      let main_def = &mut self.book.defs[entrypoint];

      if !main_def.rules[0].pats.iter().all(|pat| matches!(pat, Pattern::Var(Some(..)))) {
        self.info.def_error(entrypoint.clone(), ArgError::PatternArgError);
      }

      if let Some(args) = args {
        let expected = main_def.rules[0].pats.len();
        let got = args.len();
        if expected != got {
          self.info.error(ArgError::ArityArgError { expected, got });
        }

        main_def.convert_match_def_to_term();
        let main_body = &mut self.book.defs[entrypoint].rule_mut().body;

        *main_body = Term::call(main_body.clone(), args);
      }
    }

    self.info.fatal(())
  }
}
