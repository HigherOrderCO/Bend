use crate::term::{Book, Pattern, Term};

impl Book {
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), String> {
    if let Some(main) = &self.entrypoint
      && let Some(args) = args
    {
      let main_def = &mut self.defs[main];

      if !main_def.rules[0].pats.iter().all(|pat| matches!(pat, Pattern::Var(Some(..)))) {
        return Err("Main definition should contain only var patterns.".into());
      }

      main_def.convert_match_def_to_term();
      let main_body = &mut self.defs[main].rule_mut().body;

      *main_body = Term::call(main_body.clone(), args);
    }
    Ok(())
  }
}
