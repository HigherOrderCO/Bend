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

use crate::hvm::ast::{Net, Tree};
use core::ops::RangeFrom;
use std::collections::HashMap;

impl Net {
  /// Carries out simple eta-reduction
  pub fn eta_reduce(&mut self) {
    let mut phase1 = Phase1::default();
    for tree in self.trees() {
      phase1.walk_tree(tree);
    }
    let mut phase2 = Phase2 { nodes: phase1.nodes, index: 0 .. };
    for tree in self.trees_mut() {
      phase2.reduce_tree(tree);
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeType {
  Ctr(u16),
  Var(isize),
  Num(u32),
  Era,
  Other,
  Hole,
}

#[derive(Default)]
struct Phase1<'a> {
  vars: HashMap<&'a str, usize>,
  nodes: Vec<NodeType>,
}

impl<'a> Phase1<'a> {
  fn walk_tree(&mut self, tree: &'a Tree) {
    match tree {
      Tree::Ctr { lab, ports } => {
        let last_port = ports.len() - 1;
        for (idx, i) in ports.iter().enumerate() {
          if idx != last_port {
            self.nodes.push(NodeType::Ctr(*lab));
          }
          self.walk_tree(i);
        }
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
      Tree::Num { val } => self.nodes.push(NodeType::Num(*val)),
      _ => {
        self.nodes.push(NodeType::Other);
        for i in tree.children() {
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
  fn reduce_ctr(&mut self, lab: u16, ports: &mut Vec<Tree>, skip: usize) -> NodeType {
    if skip == ports.len() {
      return NodeType::Other;
    }
    if skip == ports.len() - 1 {
      return self.reduce_tree(&mut ports[skip]);
    }
    let head_index = self.index.next().unwrap();
    let a = self.reduce_tree(&mut ports[skip]);
    let b = self.reduce_ctr(lab, ports, skip + 1);
    if a == b {
      let reducible = match a {
        NodeType::Var(delta) => self.nodes[head_index.wrapping_add_signed(delta)] == NodeType::Ctr(lab),
        NodeType::Era | NodeType::Num(_) => true,
        _ => false,
      };
      if reducible {
        ports.pop();
        return a;
      }
    }
    NodeType::Ctr(lab)
  }
  fn reduce_tree(&mut self, tree: &mut Tree) -> NodeType {
    if let Tree::Ctr { lab, ports } = tree {
      let ty = self.reduce_ctr(*lab, ports, 0);
      if ports.len() == 1 {
        *tree = ports.pop().unwrap();
      }
      ty
    } else {
      let index = self.index.next().unwrap();
      for i in tree.children_mut() {
        self.reduce_tree(i);
      }
      self.nodes[index]
    }
  }
}
