use crate::multi_iterator;
use hvm::ast::{Net, Tree};
use std::fmt::Write;

pub mod add_recursive_priority;
pub mod check_net_size;
pub mod eta_reduce;
pub mod inline;
pub mod mutual_recursion;
pub mod prune;

pub fn tree_children(tree: &Tree) -> impl DoubleEndedIterator<Item = &Tree> + Clone {
  multi_iterator!(ChildrenIter { Zero, Two });
  match tree {
    Tree::Var { .. } | Tree::Ref { .. } | Tree::Era | Tree::Num { .. } => ChildrenIter::Zero([]),
    Tree::Con { fst, snd } | Tree::Dup { fst, snd } | Tree::Opr { fst, snd } | Tree::Swi { fst, snd } => {
      ChildrenIter::Two([fst.as_ref(), snd.as_ref()])
    }
  }
}

pub fn tree_children_mut(tree: &mut Tree) -> impl DoubleEndedIterator<Item = &mut Tree> {
  multi_iterator!(ChildrenIter { Zero, Two });
  match tree {
    Tree::Var { .. } | Tree::Ref { .. } | Tree::Era | Tree::Num { .. } => ChildrenIter::Zero([]),
    Tree::Con { fst, snd } | Tree::Dup { fst, snd } | Tree::Opr { fst, snd } | Tree::Swi { fst, snd } => {
      ChildrenIter::Two([fst.as_mut(), snd.as_mut()])
    }
  }
}

pub fn net_trees(net: &Net) -> impl DoubleEndedIterator<Item = &Tree> + Clone {
  [&net.root].into_iter().chain(net.rbag.iter().flat_map(|(_, fst, snd)| [fst, snd]))
}

pub fn net_trees_mut(net: &mut Net) -> impl DoubleEndedIterator<Item = &mut Tree> {
  [&mut net.root].into_iter().chain(net.rbag.iter_mut().flat_map(|(_, fst, snd)| [fst, snd]))
}

pub fn hvm_book_show_pretty(book: &hvm::ast::Book) -> String {
  let mut s = String::with_capacity(book.defs.len());
  for (nam, def) in book.defs.iter() {
    let _ = writeln!(&mut s, "@{} = {}", nam, def.root.show());
    for (pri, a, b) in def.rbag.iter() {
      let _ = write!(&mut s, "  &");
      if *pri {
        s.push('!');
      } else {
        s.push(' ');
      }
      let _ = writeln!(&mut s, "{} ~ {}", a.show(), b.show());
    }
    s.push('\n');
  }
  s
}
