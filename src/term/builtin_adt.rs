use indexmap::IndexMap;

use super::{Adt, Book, Name};

pub trait BuiltinAdt {
  /// The ADT constructors.
  fn constructors(&self) -> IndexMap<Name, Vec<Name>>;

  /// The ADT name.
  fn name(&self) -> Name;

  /// Implements the encoding logic for terms.
  /// Returns true if the built-in adt was used.
  fn encode_terms(&self, book: &mut Book) -> bool;

  /// Checks if the adt name or any constructor name was overridden by the user.
  fn check(&self, book: &Book) -> Result<(), String> {
    let adt_name = self.name();
    if book.adts.contains_key(&adt_name) {
      return Err(format!("{adt_name} is a built-in data type and should not be overridden."));
    }
    for (ctr, ..) in self.constructors() {
      if book.ctrs.contains_key(&ctr) {
        return Err(format!("{ctr} is a built-in constructor and should not be overridden."));
      }
    }
    Ok(())
  }

  fn declare(&self, book: &mut Book) {
    for (ctr, ..) in self.constructors() {
      book.ctrs.insert(ctr, self.name());
    }
    book.adts.insert(self.name(), Adt { ctrs: self.constructors() });
  }

  fn remove(&self, book: &mut Book) {
    for (ctr, ..) in self.constructors() {
      book.ctrs.remove(&ctr);
    }
    book.adts.remove(&self.name());
  }
}

impl Book {
  pub fn encode_builtin_adt(&mut self, btin_adt: impl BuiltinAdt) -> Result<(), String> {
    btin_adt.check(self)?;

    btin_adt.declare(self);

    if !btin_adt.encode_terms(self) {
      btin_adt.remove(self);
    }

    Ok(())
  }
}
