use crate::term::{Book, Term};

impl Book {
  /// Inline copies of the declared bind in the `use` expression.
  ///
  /// Example:
  /// ```hvml
  /// use id = λx x
  /// (id id id)
  ///
  /// // Transforms to:
  /// (λx x λx x λx x)
  /// ```
  pub fn apply_use(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.apply_use();
      }
    }
  }
}

impl Term {
  pub fn apply_use(&mut self) {
    Term::recursive_call(move || match self {
      Term::Use { nam, val, nxt } => {
        val.apply_use();
        nxt.apply_use();

        nxt.subst(nam, val);
        *self = std::mem::take(nxt);
      }
      other => {
        for children in other.children_mut() {
          children.apply_use();
        }
      }
    });
  }
}
