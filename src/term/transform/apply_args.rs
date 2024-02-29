use crate::term::{Book, Pattern, Term};

impl Book {
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), String> {
    if let Some(entrypoint) = &self.entrypoint
      && let Some(args) = args
    {
      let main_def = &mut self.defs[entrypoint];
      let expected = main_def.rules[0].pats.len();
      let got = args.len();

      if !main_def.rules[0].pats.iter().all(|pat| matches!(pat, Pattern::Var(Some(..)))) {
        return Err("Definition '{entrypoint}' should contain only var patterns.".into());
      }

      if expected != got {
        return Err(format!("Expected {expected} arguments, got {got}."));
      }

      main_def.convert_match_def_to_term();
      let main_body = &mut self.defs[entrypoint].rule_mut().body;

      *main_body = Term::call(main_body.clone(), args);
    }
    Ok(())
  }
}
