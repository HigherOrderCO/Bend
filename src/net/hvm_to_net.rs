use super::{INet, INode, INodes, NodeId, NodeKind::*, Port, SlotId, ROOT};
use crate::hvm::ast::{Net, Tree};
use crate::{fun::Name, net::NodeKind};

pub fn hvm_to_net(net: &Net) -> INet {
  let inodes = hvm_to_inodes(net);
  inodes_to_inet(&inodes)
}

fn hvm_to_inodes(net: &Net) -> INodes {
  let mut inodes = vec![];
  let mut n_vars = 0;
  let net_root = if let Tree::Var { nam } = &net.root { nam } else { "" };

  // If we have a tree attached to the net root, convert that first
  if !matches!(&net.root, Tree::Var { .. }) {
    let mut root = tree_to_inodes(&net.root, "_".to_string(), net_root, &mut n_vars);
    inodes.append(&mut root);
  }

  // Convert all the trees forming active pairs.
  for (i, (_, tree1, tree2)) in net.rbag.iter().enumerate() {
    // This name cannot appear anywhere in the original net
    let tree_root = format!("%a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars);
    inodes.append(&mut tree2);
  }
  inodes
}

fn new_var(n_vars: &mut NodeId) -> String {
  // This name cannot appear anywhere in the original net
  let new_var = format!("%x{n_vars}");
  *n_vars += 1;
  new_var
}

fn tree_to_inodes(tree: &Tree, tree_root: String, net_root: &str, n_vars: &mut NodeId) -> INodes {
  fn process_node_subtree<'a>(
    subtree: &'a Tree,
    net_root: &str,
    subtrees: &mut Vec<(String, &'a Tree)>,
    n_vars: &mut NodeId,
  ) -> String {
    if let Tree::Var { nam } | Tree::Sub { nam } = subtree {
      if nam == net_root {
        "_".to_string()
      } else {
        nam.clone()
      }
    } else {
      let var = new_var(n_vars);
      subtrees.push((var.clone(), subtree));
      var
    }
  }

  let mut inodes = vec![];
  let mut subtrees = vec![(tree_root, tree)];
  while let Some((subtree_root, subtree)) = subtrees.pop() {
    match subtree {
      Tree::Era => {
        let var = new_var(n_vars);
        inodes.push(INode { kind: Era, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Del => {
        let var = new_var(n_vars);
        inodes.push(INode { kind: Del, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Lam { fst, snd } => {
        let kind = NodeKind::Lam;
        let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
        let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
      }
      Tree::App { fst, snd } => {
        let kind = NodeKind::App;
        let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
        let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
      }
      Tree::Sup { fst, snd } => {
        let kind = NodeKind::Sup;
        let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
        let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
      }
      Tree::Dup { fst, snd } => {
        let kind = NodeKind::Dup;
        let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
        let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
      }
      Tree::Var { .. } => unreachable!(),
      Tree::Sub { .. } => unreachable!(),
      Tree::Ref { nam } => {
        let kind = Ref { def_name: Name::new(nam) };
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      } /* Tree::Num { val } => {
          let kind = Num { val: val.0 };
          let var = new_var(n_vars);
          inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
        }
        Tree::Opr { fst, snd } => {
          let kind = NodeKind::Opr;
          let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
          let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
          inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
        }
        Tree::Swi { fst, snd } => {
          let kind = NodeKind::Swi;
          let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
          let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
          inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
        } */
    }
  }
  inodes
}

// Converts INodes to an INet by linking ports based on names.
fn inodes_to_inet(inodes: &INodes) -> INet {
  let mut inet = INet::new();
  // Maps named inode ports to numeric inet ports.
  let mut name_map = std::collections::HashMap::new();

  for inode in inodes {
    let node = inet.new_node(inode.kind.clone());
    for (j, name) in inode.ports.iter().enumerate() {
      let p = Port(node, j as SlotId);
      if name == "_" {
        inet.link(p, ROOT);
      } else if let Some(&q) = name_map.get(name) {
        inet.link(p, q);
        name_map.remove(name);
      } else {
        name_map.insert(name.clone(), p);
      }
    }
  }

  inet
}
