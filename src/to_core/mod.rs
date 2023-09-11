pub mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{core::Book, DefId, Definition, DefinitionBook, Name, Term};
use hvm_core::{lnet_to_net, LNet};
use std::collections::{HashMap, HashSet};

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<Book> {
  let mut core_book = Book { defs: HashMap::new() };
  let def_names = book.defs.values().map(|x| &x.name).cloned().collect();
  for (def_id, def) in book.defs.iter() {
    let net = definition_to_hvm_core(def, &def_names)?;
    core_book.defs.insert(*def_id, net);
  }
  Ok(core_book)
}

pub fn definition_to_hvm_core(definition: &Definition, def_names: &HashSet<Name>) -> anyhow::Result<LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(definition.rules[0].body.clone(), def_names)
}

pub fn term_to_hvm_core(term: Term, def_names: &HashSet<Name>) -> anyhow::Result<LNet> {
  let term = term.try_into_affine(def_names)?;
  let compat_net = term_to_compat_net(&term, def_names)?;
  compat_net_to_core(&compat_net)
}

pub fn book_to_hvm_internal(book: &Book) -> anyhow::Result<(hvm_core::Net, hvm_core::Book)> {
  // TODO: Don't try to preallocate a huge buffer
  let mut root = hvm_core::Net::new(1 << 26);
  root.boot(*DefId::from("Main"));

  let mut hvm_book = hvm_core::Book::new();
  book.defs.iter().for_each(|(&name, term)| {
    hvm_book.def(*name, lnet_to_net(term));
  });

  Ok((root, hvm_book))
}
