mod affine;
mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{
  core::{Book, LNet},
  DefId, Definition, DefinitionBook, Name, Term,
};
use hvm2::lang::name_to_u64;
use std::collections::HashMap;

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<Book> {
  let def_to_id = HashMap::from_iter(book.defs.keys().map(|name| (name.clone(), DefId(name_to_u64(name)))));
  let mut core_book = Book { defs: HashMap::new() };
  for (name, def) in book.defs.iter() {
    let net = definition_to_hvm_core(def, &def_to_id)?;
    core_book.defs.insert(def_to_id[name], net);
  }
  Ok(core_book)
}

pub fn definition_to_hvm_core(
  definition: &Definition,
  def_to_id: &HashMap<Name, DefId>,
) -> anyhow::Result<LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(definition.rules[0].body.clone(), def_to_id)
}

pub fn term_to_hvm_core(term: Term, def_to_id: &HashMap<Name, DefId>) -> anyhow::Result<LNet> {
  let term = term.try_into_affine(&def_to_id.keys().cloned().collect())?;
  let compat_net = term_to_compat_net(&term, def_to_id)?;
  compat_net_to_core(&compat_net)
}
