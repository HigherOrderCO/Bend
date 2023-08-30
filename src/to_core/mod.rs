mod compat_net;

use self::compat_net::term_to_compat_net;
use crate::ast::{
  self,
  core::{name_to_u32, LNet},
  DefId, Definition, DefinitionBook, Name,
};
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

fn definition_to_hvm_core(
  definition: &Definition,
  def_to_id: &HashMap<Name, DefId>,
) -> anyhow::Result<ast::core::LNet> {
  let term = &definition.rules[0].body;
  let compat_net = term_to_compat_net(term, def_to_id)?;
  compat_net_to_core(compat_net)
}

fn compat_net_to_core(compat_net: compat_net::INet) -> anyhow::Result<LNet> {
  todo!()
}
