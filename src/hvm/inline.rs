use super::{net_trees_mut, tree_children, tree_children_mut};
use crate::maybe_grow;
use core::ops::BitOr;
use hvm::ast::{Book, Net, Tree};
use std::collections::{HashMap, HashSet};

pub fn inline_hvm_book(book: &mut Book) -> Result<HashSet<String>, String> {
  let mut state = InlineState::default();
  state.populate_inlinees(book)?;
  let mut all_changed = HashSet::new();
  for (name, net) in &mut book.defs {
    let mut inlined = false;
    for tree in net_trees_mut(net) {
      inlined |= state.inline_into(tree);
    }
    if inlined {
      all_changed.insert(name.to_owned());
    }
  }
  Ok(all_changed)
}

#[derive(Debug, Default)]
struct InlineState {
  inlinees: HashMap<String, Tree>,
}

impl InlineState {
  fn populate_inlinees(&mut self, book: &Book) -> Result<(), String> {
    for (name, net) in &book.defs {
      if should_inline(net) {
        // Detect cycles with tortoise and hare algorithm
        let mut hare = &net.root;
        let mut tortoise = &net.root;
        // Whether or not the tortoise should take a step
        let mut parity = false;
        while let Tree::Ref { nam, .. } = hare {
          let Some(net) = &book.defs.get(nam) else { break };
          if should_inline(net) {
            hare = &net.root;
          } else {
            break;
          }
          if parity {
            let Tree::Ref { nam: tortoise_nam, .. } = tortoise else { unreachable!() };
            if tortoise_nam == nam {
              Err(format!("infinite reference cycle in `@{nam}`"))?;
            }
            tortoise = &book.defs[tortoise_nam].root;
          }
          parity = !parity;
        }
        self.inlinees.insert(name.to_owned(), hare.clone());
      }
    }
    Ok(())
  }
  fn inline_into(&self, tree: &mut Tree) -> bool {
    maybe_grow(|| {
      let Tree::Ref { nam, .. } = &*tree else {
        return tree_children_mut(tree).map(|t| self.inline_into(t)).fold(false, bool::bitor);
      };
      if let Some(inlined) = self.inlinees.get(nam) {
        *tree = inlined.clone();
        true
      } else {
        false
      }
    })
  }
}

fn should_inline(net: &Net) -> bool {
  net.rbag.is_empty() && tree_children(&net.root).next().is_none()
}
