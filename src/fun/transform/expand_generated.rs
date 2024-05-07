use crate::{
  fun::{Book, Term},
  maybe_grow,
};

/// Dereferences any generated definitions in the term.
/// Used after readback.
impl Term {
  pub fn expand_generated(&mut self, book: &Book) {
    maybe_grow(|| {
      if let Term::Ref { nam } = &*self {
        if nam.contains("__") {
          *self = book.defs.get(nam).unwrap().rule().body.clone();
        }
      }

      // Note: this assumes that there will never be a loop of generated functions.
      // This is true right now, but not necessarily in the future.
      for child in self.children_mut() {
        child.expand_generated(book);
      }
    })
  }
}
