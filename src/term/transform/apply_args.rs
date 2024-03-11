use crate::{
  diagnostics::{Diagnostics, ToStringVerbose},
  term::{Ctx, Pattern, Term},
};

struct PatternArgError(Pattern);

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
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), Diagnostics> {
    self.info.start_pass();

    if let Some(entrypoint) = &self.book.entrypoint {
      let main_def = &mut self.book.defs[entrypoint];

      for pat in &main_def.rules[0].pats {
        if !matches!(pat, Pattern::Var(Some(..))) {
          self.info.add_rule_error(PatternArgError(pat.clone()), entrypoint.clone());
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

impl ToStringVerbose for PatternArgError {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    format!("Expected the entrypoint to only have variable pattern, found '{}'.", self.0)
  }
}
