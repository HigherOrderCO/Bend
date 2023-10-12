pub mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{DefId, Definition, DefinitionBook, Term};
use hvmc::{lbook_to_book, val_to_name, LBook, LNet};

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<LBook> {
  let mut lbook = LBook::new();
  for def in book.defs.iter() {
    let net = definition_to_hvm_core(def)?;
    lbook.insert(val_to_name(def.def_id.to_internal()), net);
  }
  Ok(lbook)
}

pub fn definition_to_hvm_core(definition: &Definition) -> anyhow::Result<LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(&definition.rules[0].body)
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

/*
pub fn term_to_hvm_core2(term: &Term, def_names: &HashSet<Name>) -> anyhow::Result<LNet> {

}

fn go(term: &Term, def_names: &HashSet<Name>, prev: Option<Name>, gen_var_count: &mut usize) -> (Option<LTree>, Option<(LTree, LTree)>) {
  match term {
    Term::Lam { nam, bod } => todo!(),
    Term::Var { nam } => todo!(),
    Term::Ref { def_id } => LTree::Ref { nam: **def_id },
    Term::App { fun, arg } => {
      if let Some(prev) = prev {
        let fun =
        let app = LTree::Nod { tag: CON, lft: (), rgt: LTree::Var { nam: prev } };
      } else {
        unreachable!();
      }
    }
    Term::Dup { fst, snd, val, nxt } => todo!(),
    Term::Num { val } => LTree::NUM { val },
    Term::NumOp { op, fst, snd } => unreachable!(),
    Term::Sup { fst, snd } => unreachable!(),
    Term::Era => unreachable!(),
}
}*/
