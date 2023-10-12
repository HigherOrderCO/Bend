use crate::{
  net::{hvmcLNet_to_Net::core_net_to_compat, net_to_hvmcLNet::compat_net_to_core},
  term::{net_to_term::readback_compat, term_to_net::term_to_compat_net, *},
};
use hvmc::{lbook_to_book, val_to_name, LBook, LNet};

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<LBook> {
  let mut lbook = LBook::new();
  for rule in book.defs.iter() {
    let net = term_to_hvm_core(&rule.body)?;
    lbook.insert(val_to_name(rule.def_id.to_internal()), net);
  }
  Ok(lbook)
}

pub fn term_to_hvm_core(term: &Term) -> anyhow::Result<LNet> {
  let compat_net = term_to_compat_net(term)?;
  compat_net_to_core(&compat_net)
}

pub fn book_to_hvm_internal(book: &LBook, main: DefId, mem_size: usize) -> (hvmc::Net, hvmc::Book) {
  let book = lbook_to_book(book);
  let mut net = hvmc::Net::new(mem_size);
  net.boot(main.to_internal());
  (net, book)
}

pub fn readback_net(net: &LNet, book: &DefinitionBook) -> anyhow::Result<(Term, bool)> {
  /* check_lnet_valid(net)?; */
  let compat_net = core_net_to_compat(net)?;
  let readback = readback_compat(&compat_net, book);
  Ok(readback)
}
