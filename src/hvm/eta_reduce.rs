//! Carries out simple eta-reduction, to reduce the amount of rewrites at
//! runtime.
//!
//! ### Eta-equivalence
//!
//! In interaction combinators, there are some nets that are equivalent and
//! have no observable difference
//!
//! ![Image of eta-equivalence](https://i.postimg.cc/XYVxdMFW/image.png)
//!
//! This module implements the eta-equivalence rule at the top-left of the image
//! above
//!
//! ```txt
//!     /|-, ,-|\     eta_reduce
//! ---| |  X  | |-- ~~~~~~~~~~~~> -------------
//!     \|-' '-|/
//! ```
//!
//! In hvm-core's AST representation, this reduction looks like this
//!
//! ```txt
//! {lab x y} ... {lab x y} ~~~~~~~~> x ..... x
//! ```
//!
//! Essentially, both occurrences of the same constructor are replaced by a
//! variable.
//!
//! ### The algorithm
//!
//! The code uses a two-pass O(n) algorithm, where `n` is the amount of nodes
//! in the AST
//!
//! In the first pass, a node-list is built out of an ordered traversal of the
//! AST. Crucially, the node list stores variable offsets instead of the
//! variable's names Since the AST's order is consistent, the ordering of nodes
//! in the node list can be reproduced with a traversal.
//!
//! This means that each occurrence of a variable is encoded with the offset in
//! the node-list to the _other_ occurrence of the variable.
//!
//! For example, if we start with the net: `[(x y) (x y)]`
//!
//! The resulting node list will look like this:
//!
//! `[Ctr(1), Ctr(0), Var(3), Var(3), Ctr(0), Var(-3), Var(-3)]`
//!
//! The second pass uses the node list to find repeated constructors. If a
//! constructor's children are both variables with the same offset, then we
//! lookup that offset relative to the constructor. If it is equal to the first
//! constructor, it means both of them are equal and they can be replaced with a
//! variable.
//!
//! The pass also reduces subnets such as `(* *) -> *`

use crate::hvm::net_trees_mut;

use super::{tree_children, tree_children_mut};
use crate::hvm::ast::{Net, Tree};
use core::ops::RangeFrom;
use std::collections::HashMap;

/// Carries out simple eta-reduction
pub fn eta_reduce_hvm_net(net: &mut Net) {
  let mut phase1 = Phase1::default();
  for tree in net_trees_mut(net) {
    phase1.walk_tree(tree);
  }
  let mut phase2 = Phase2 { nodes: phase1.nodes, index: 0.. };
  for tree in net_trees_mut(net) {
    phase2.reduce_tree(tree);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeType {
  Ctr(u16),
  Var(isize),
  Era,
  Other,
  Hole,
}

#[derive(Default, Debug)]
struct Phase1<'a> {
  vars: HashMap<&'a str, usize>,
  nodes: Vec<NodeType>,
}

impl<'a> Phase1<'a> {
  fn walk_tree(&mut self, tree: &'a Tree) {
    match tree {
      Tree::Con { fst, snd } => {
        self.nodes.push(NodeType::Ctr(0));
        self.walk_tree(fst);
        self.walk_tree(snd);
      }
      Tree::Dup { fst, snd } => {
        self.nodes.push(NodeType::Ctr(1));
        self.walk_tree(fst);
        self.walk_tree(snd);
      }
      Tree::Var { nam } => {
        if let Some(i) = self.vars.get(&**nam) {
          let j = self.nodes.len() as isize;
          self.nodes.push(NodeType::Var(*i as isize - j));
          self.nodes[*i] = NodeType::Var(j - *i as isize);
        } else {
          self.vars.insert(nam, self.nodes.len());
          self.nodes.push(NodeType::Hole);
        }
      }
      Tree::Era => self.nodes.push(NodeType::Era),
      _ => {
        self.nodes.push(NodeType::Other);
        for i in tree_children(tree) {
          self.walk_tree(i);
        }
      }
    }
  }
}

struct Phase2 {
  nodes: Vec<NodeType>,
  index: RangeFrom<usize>,
}

impl Phase2 {
  fn reduce_ctr(&mut self, tree: &mut Tree, idx: usize) -> NodeType {
    if let Tree::Con { fst, snd } | Tree::Dup { fst, snd } = tree {
      let fst_typ = self.reduce_tree(fst);
      let snd_typ = self.reduce_tree(snd);
      // If both children are variables with the same offset, and their parent is a ctr of the same label,
      // then they are eta-reducible and we replace the current node with the first variable.
      match (fst_typ, snd_typ) {
        (NodeType::Var(off_lft), NodeType::Var(off_rgt)) => {
          if off_lft == off_rgt && self.nodes[idx] == self.nodes[(idx as isize + off_lft) as usize] {
            let Tree::Var { nam } = fst.as_mut() else { unreachable!() };
            *tree = Tree::Var { nam: std::mem::take(nam) };
            return NodeType::Var(off_lft);
          }
        }
        (NodeType::Era, NodeType::Era) => {
          *tree = Tree::Era;
          return NodeType::Era;
        }
        _ => {}
      }
      self.nodes[idx]
    } else {
      unreachable!()
    }
  }

  fn reduce_tree(&mut self, tree: &mut Tree) -> NodeType {
    let idx = self.index.next().unwrap();
    match tree {
      Tree::Con { .. } | Tree::Dup { .. } => self.reduce_ctr(tree, idx),
      _ => {
        for child in tree_children_mut(tree) {
          self.reduce_tree(child);
        }
        self.nodes[idx]
      }
    }
  }
}
