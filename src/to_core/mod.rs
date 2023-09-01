mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{self, core::name_to_u32, DefId, Definition, DefinitionBook, Name, Term};
use std::collections::HashMap;

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<ast::core::Book> {
  let def_to_id = HashMap::from_iter(book.defs.keys().map(|name| (name.clone(), DefId(name_to_u32(name)))));
  let mut core_book = ast::core::Book { defs: HashMap::new() };
  for (name, def) in book.defs.iter() {
    let net = definition_to_hvm_core(def, &def_to_id)?;
    core_book.defs.insert(def_to_id[name], net);
  }
  Ok(core_book)
}

pub fn definition_to_hvm_core(
  definition: &Definition,
  def_to_id: &HashMap<Name, DefId>,
) -> anyhow::Result<ast::core::LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(&definition.rules[0].body, def_to_id)
}

pub fn term_to_hvm_core(term: &Term, def_to_id: &HashMap<Name, DefId>) -> anyhow::Result<ast::core::LNet> {
  let compat_net = term_to_compat_net(term, def_to_id)?;
  compat_net_to_core(&compat_net)
}
