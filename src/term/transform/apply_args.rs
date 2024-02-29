use crate::term::{Book, Pattern, Term};

impl Book {
  pub fn apply_args(&mut self, args: Option<Vec<Term>>) -> Result<(), String> {
    if let Some(main) = &self.entrypoint
      && let Some(args) = args
    {
      let mut args = args.into_iter();
      let main_rule = &mut self.defs[main].rules[0];
      let main_body = &mut main_rule.body;

      for pat in &main_rule.pats {
        if let Pattern::Var(Some(x)) = pat {
          main_body.subst(x, &args.next().unwrap());
        } else {
          return Err(format!("Expected a variable pattern, but found '{pat}'."));
        }
      }

      main_rule.pats.clear();
    }
    Ok(())
  }
}
