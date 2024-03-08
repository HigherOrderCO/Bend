use std::fmt::Display;

use crate::{
  diagnostics::Info,
  term::{Ctx, Pattern, Term},
};

#[derive(Clone, Debug)]
pub struct PatternArgError(Pattern);

impl Display for PatternArgError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Expected a variable pattern, found '{}'.", self.0)
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

      for pat in &main_def.rules[0].pats {
        if !matches!(pat, Pattern::Var(Some(..))) {
          self.info.def_error(entrypoint.clone(), PatternArgError(pat.clone()));
        }
      }

      if let Some(args) = args {
        main_def.convert_match_def_to_term();
        let main_body = &mut self.book.defs[entrypoint].rule_mut().body;

        *main_body = Term::call(main_body.clone(), args);
      }
    }

    self.info.fatal(())
  }
}
