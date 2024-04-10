use crate::{
  maybe_grow,
  term::{Book, Term},
};

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
    maybe_grow(|| {
      for children in self.children_mut() {
        children.apply_use();
      }
    });

    if let Term::Use { nam: Some(nam), val, nxt } = self {
      nxt.subst(nam, val);
      *self = std::mem::take(nxt);
    }
  }
}
