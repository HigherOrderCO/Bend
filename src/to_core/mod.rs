pub mod compat_net;

use self::compat_net::{compat_net_to_core, term_to_compat_net};
use crate::ast::{core::Book, DefId, Definition, DefinitionBook, Name, Term};
use hvm_core::{lnet_to_net, LNet};
use std::collections::HashMap;

pub fn book_to_hvm_core(book: &DefinitionBook) -> anyhow::Result<Book> {
  let mut defs = HashMap::new();
  for def in book.defs.iter() {
    let net = definition_to_hvm_core(def)?;
    defs.insert(def.def_id, net);
  }
  // TODO: Ugly, if we know whether main will be there we should use that information correctly
  let main = *book.def_names.get_by_right(&Name::from("Main".to_string())).unwrap_or(&DefId(0));
  Ok(Book { defs, main })
}

pub fn definition_to_hvm_core(definition: &Definition) -> anyhow::Result<LNet> {
  // TODO: Multiple rules, pattern matching, etc.
  term_to_hvm_core(&definition.rules[0].body)
}

pub fn term_to_hvm_core(term: &Term) -> anyhow::Result<LNet> {
  let compat_net = term_to_compat_net(term)?;
  compat_net_to_core(&compat_net)
}

pub fn book_to_hvm_internal(book: &Book, mem_size: usize) -> anyhow::Result<(hvm_core::Net, hvm_core::Book)> {
  // TODO: Don't try to preallocate a huge buffer
  let mut root = hvm_core::Net::new(mem_size);
  root.boot(book.main.to_internal()); // TODO: Don't use this workaround

  let mut hvm_book = hvm_core::Book::new();
  book.defs.iter().for_each(|(&def_id, term)| {
    hvm_book.def(def_id.to_internal(), lnet_to_net(term, None).to_def());
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
