use super::{net_trees, tree_children};
use crate::hvm::ast::{Book, Tree};
use crate::maybe_grow;
use std::collections::HashSet;

pub fn prune_hvm_book(book: &mut Book, entrypoints: &[String]) {
  let mut state = PruneState { book, unvisited: book.defs.keys().map(|x| x.to_owned()).collect() };
  for name in entrypoints {
    state.visit_def(name);
  }
  let unvisited = state.unvisited;
  for name in unvisited {
    book.defs.remove(&name);
  }
}

struct PruneState<'a> {
  book: &'a Book,
  unvisited: HashSet<String>,
}

impl<'a> PruneState<'a> {
  fn visit_def(&mut self, name: &str) {
    if self.unvisited.remove(name) {
      for tree in net_trees(&self.book.defs[name]) {
        self.visit_tree(tree);
      }
    }
  }
  fn visit_tree(&mut self, tree: &Tree) {
    maybe_grow(|| {
      if let Tree::Ref { nam, .. } = tree {
        self.visit_def(nam);
      } else {
        tree_children(tree).for_each(|t| self.visit_tree(t));
      }
    })
  }
}
