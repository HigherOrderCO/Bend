use std::collections::HashSet;

use crate::{
  hvm::ast::{Book, Tree},
  maybe_grow,
};

impl Book {
  pub fn prune(&mut self, entrypoints: &[String]) {
    let mut state = PruneState { book: self, unvisited: self.keys().map(|x| x.to_owned()).collect() };
    for name in entrypoints {
      state.visit_def(name);
    }
    let unvisited = state.unvisited;
    for name in unvisited {
      self.remove(&name);
    }
  }
}

#[derive(Debug)]
struct PruneState<'a> {
  book: &'a Book,
  unvisited: HashSet<String>,
}

impl<'a> PruneState<'a> {
  fn visit_def(&mut self, name: &str) {
    if self.unvisited.remove(name) {
      for tree in self.book[name].trees() {
        self.visit_tree(tree);
      }
    }
  }
  fn visit_tree(&mut self, tree: &Tree) {
    maybe_grow(|| {
      if let Tree::Ref { nam, .. } = tree {
        self.visit_def(nam);
      } else {
        tree.children().for_each(|t| self.visit_tree(t));
      }
    })
  }
}
