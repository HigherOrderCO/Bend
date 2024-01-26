use crate::term::Book;

impl Book {
  // Checks for both unbound patterns and unbound variables.
  pub fn check_unbounds(&self) -> Result<(), String> {
    self.check_unbound_vars()?;
    self.check_unbound_pats()
  }
}
