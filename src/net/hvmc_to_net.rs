use super::inter_net::{
  link, new_inet, new_node, port, INet, INode, INodes, NodeId, NodeKind, SlotId, CON, DUP, ERA, NUM, OP2,
  REF, ROOT,
};
use crate::{net::inter_net::MAT, term::DefId};
use hvmc::ast::{Net, Tree};

pub fn core_net_to_compat(net: &Net) -> anyhow::Result<INet> {
  let inodes = hvmc_to_inodes(net);
  let compat_net = inodes_to_inet(&inodes);
  Ok(compat_net)
}

fn hvmc_to_inodes(net: &Net) -> INodes {
  let mut inodes = vec![];
  let mut n_vars = 0;
  let net_root = if let Tree::Var { nam } = &net.root { nam } else { "" };

  // If we have a tree attached to the net root, convert that first
  if !matches!(&net.root, Tree::Var { .. }) {
    let mut root = tree_to_inodes(&net.root, "_".to_string(), net_root, &mut n_vars);
    inodes.append(&mut root);
  }
  // Convert all the trees forming active pairs.
  for (i, (tree1, tree2)) in net.rdex.iter().enumerate() {
    let tree_root = format!("a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars);
    inodes.append(&mut tree2);
  }
  inodes
}

fn tree_to_inodes(tree: &Tree, tree_root: String, net_root: &str, n_vars: &mut NodeId) -> INodes {
  fn new_var(n_vars: &mut NodeId) -> String {
    let new_var = format!("x{n_vars}");
    *n_vars += 1;
    new_var
  }

  fn process_node_subtree<'a>(
    subtree: &'a Tree,
    net_root: &str,
    subtrees: &mut Vec<(String, &'a Tree)>,
    n_vars: &mut NodeId,
  ) -> String {
    if let Tree::Var { nam } = subtree {
      if nam == net_root { "_".to_string() } else { nam.clone() }
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
        inodes.push(INode { kind: ERA, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Ctr { lab, lft, rgt } => {
        let kind = if *lab == 0 { CON } else { DUP | (*lab - 1) as NodeKind };
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, lft, rgt] })
      }
      Tree::Var { .. } => unreachable!(),
      Tree::Ref { nam } => {
        let kind = REF | (*DefId::from_internal(*nam) as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Num { val } => {
        let kind = NUM | (*val as NodeKind);
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Op2 { lft, rgt } => {
        let kind = OP2;
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, lft, rgt] })
      }
      Tree::Mat { sel, ret } => {
        let kind = MAT;
        let sel = process_node_subtree(sel, net_root, &mut subtrees, n_vars);
        let ret = process_node_subtree(ret, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, sel, ret] })
      }
    }
  }
  inodes
}

// Converts INodes to an INet by linking ports based on names.
fn inodes_to_inet(inodes: &INodes) -> INet {
  let mut inet = new_inet();
  let mut name_map = std::collections::HashMap::new();

  for inode in inodes.iter() {
    let node = new_node(&mut inet, inode.kind);
    for (j, name) in inode.ports.iter().enumerate() {
      let p = port(node, j as SlotId);
      if name == "_" {
        link(&mut inet, p, ROOT);
      } else if let Some(&q) = name_map.get(name) {
        link(&mut inet, p, q);
        name_map.remove(name);
      } else {
        name_map.insert(name.clone(), p);
      }
    }
  }

  inet
}
