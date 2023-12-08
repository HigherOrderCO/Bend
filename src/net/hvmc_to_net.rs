use super::{INet, INode, INodes, NodeId, NodeKind::*, Port, SlotId, ROOT};
use crate::term::DefId;
use hvmc::{
  ast::{Net, Tree},
  run::Val,
};

pub fn hvmc_to_net(net: &Net, hvmc_name_to_id: &impl Fn(Val) -> DefId) -> INet {
  let inodes = hvmc_to_inodes(net, hvmc_name_to_id);
  inodes_to_inet(&inodes)
}

fn hvmc_to_inodes(net: &Net, hvmc_name_to_id: &impl Fn(Val) -> DefId) -> INodes {
  let mut inodes = vec![];
  let mut n_vars = 0;
  let net_root = if let Tree::Var { nam } = &net.root { nam } else { "" };

  // If we have a tree attached to the net root, convert that first
  if !matches!(&net.root, Tree::Var { .. }) {
    let mut root = tree_to_inodes(&net.root, "_".to_string(), net_root, &mut n_vars, hvmc_name_to_id);
    inodes.append(&mut root);
  }
  // Convert all the trees forming active pairs.
  for (i, (tree1, tree2)) in net.rdex.iter().enumerate() {
    let tree_root = format!("a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars, hvmc_name_to_id);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars, hvmc_name_to_id);
    inodes.append(&mut tree2);
  }
  inodes
}

fn tree_to_inodes(
  tree: &Tree,
  tree_root: String,
  net_root: &str,
  n_vars: &mut NodeId,
  hvmc_name_to_id: &impl Fn(Val) -> DefId,
) -> INodes {
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
        inodes.push(INode { kind: Era, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Con { lft, rgt } => {
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind: Con { lab: None }, ports: [subtree_root, lft, rgt] })
      }
      Tree::Tup { lft, rgt } => {
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind: Tup, ports: [subtree_root, lft, rgt] })
      }
      Tree::Dup { lab, lft, rgt } => {
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode {
          kind: if lab & 1 == 0 { Con { lab: Some((lab >> 1) - 1) } } else { Dup { lab: (lab >> 1) - 1 } },
          ports: [subtree_root, lft, rgt],
        })
      }
      Tree::Var { .. } => unreachable!(),
      Tree::Ref { nam } => {
        let kind = Ref { def_id: hvmc_name_to_id(*nam) };
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Num { val } => {
        let kind = Num { val: *val };
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Op1 { opr, lft, rgt } => {
        // Add port 1 as a new Num node.
        let lft_name = new_var(n_vars);
        let num_name = new_var(n_vars);
        inodes.push(INode { kind: Num { val: *lft }, ports: [lft_name.clone(), num_name.clone(), num_name] });
        // Swap ports 0 and 1 and transform into OP2.
        let kind = Op2 { opr: *opr };
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [lft_name, subtree_root, rgt] })
      }
      Tree::Op2 { opr, lft, rgt } => {
        let kind = Op2 { opr: *opr };
        let lft = process_node_subtree(lft, net_root, &mut subtrees, n_vars);
        let rgt = process_node_subtree(rgt, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, lft, rgt] })
      }
      Tree::Mat { sel, ret } => {
        let kind = Mat;
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
  let mut inet = INet::new();
  // Maps named inode ports to numeric inet ports.
  let mut name_map = std::collections::HashMap::new();

  for inode in inodes.iter() {
    let node = inet.new_node(inode.kind);
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
