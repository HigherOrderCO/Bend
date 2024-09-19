use super::{net_trees_mut, tree_children, tree_children_mut};
use crate::maybe_grow;
use core::ops::BitOr;
use hvm::ast::{Book, Net, Tree};
use std::collections::{BTreeMap, HashSet};
use std::mem;

pub fn inline_hvm_book(book: &mut Book) -> Result<HashSet<String>, String> {
  let mut state = InlineState::default();
  state.populate_inlinees(book)?;

  let mut all_changed = HashSet::new();
  let mut new_defs = BTreeMap::new();

  for (name, mut net) in mem::take(&mut book.defs) {
    let mut inlined = false;
    for tree in net_trees_mut(&mut net) {
      inlined |= state.inline_into(tree);
    }
    if inlined {
      all_changed.insert(name.clone());
    }
    new_defs.insert(name, net);
  }

  book.defs = new_defs;
  Ok(all_changed)
}

#[derive(Debug, Default)]
struct InlineState {
  inlinees: BTreeMap<String, Tree>,
}

impl InlineState {
  fn populate_inlinees(&mut self, book: &Book) -> Result<(), String> {
    for (name, net) in &book.defs {
      if should_inline(net) {
        let hare = self.find_inlineable(book, name)?;
        self.inlinees.insert(name.to_owned(), hare.clone());
      }
    }
    Ok(())
  }

  fn find_inlineable<'a>(&self, book: &'a Book, start: &str) -> Result<&'a Tree, String> {
    let mut hare = &book.defs[start].root;
    let mut tortoise = hare; // Detect cycles with tortoise and hare algorithm
    let mut parity = false; // Whether or not the tortoise should take a step
    while let Tree::Ref { nam, .. } = hare {
      let Some(net) = book.defs.get(nam) else { break };
      if !should_inline(net) {
        break;
      }
      hare = &net.root;
      if parity {
        if let Tree::Ref { nam: tortoise_nam, .. } = tortoise {
          if tortoise_nam == nam {
            return Err(format!("infinite reference cycle in `@{nam}`"));
          }
          tortoise = &book.defs[tortoise_nam].root;
        }
      }
      parity = !parity;
    }
    Ok(hare)
  }

  fn inline_into(&self, tree: &mut Tree) -> bool {
    maybe_grow(|| match tree {
      Tree::Ref { nam, .. } => {
        if let Some(inlined) = self.inlinees.get(nam) {
          *tree = inlined.clone();
          true
        } else {
          false
        }
      }
      _ => tree_children_mut(tree).map(|t| self.inline_into(t)).fold(false, BitOr::bitor),
    })
  }
}

fn should_inline(net: &Net) -> bool {
  net.rbag.is_empty() && tree_children(&net.root).next().is_none()
}
