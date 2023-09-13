pub mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{core::Book, DefId, Definition, DefinitionBook, Term};
use hvm_core::{lnet_to_net, LNet};
use std::collections::HashMap;

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<Book> {
  let mut core_book = Book { defs: HashMap::new() };
  for (def_id, def) in book.defs.iter() {
    let net = definition_to_hvm_core(def)?;
    core_book.defs.insert(*def_id, net);
  }
  Ok(core_book)
}

pub fn definition_to_hvm_core(definition: &Definition) -> anyhow::Result<LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(&definition.rules[0].body)
}

pub fn term_to_hvm_core(term: &Term) -> anyhow::Result<LNet> {
  let compat_net = term_to_compat_net(term)?;
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
