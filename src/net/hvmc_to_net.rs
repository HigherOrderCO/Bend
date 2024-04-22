use super::{INet, INode, INodes, NodeId, NodeKind::*, Port, SlotId, ROOT};
use crate::{
  net::{CtrKind, NodeKind},
  term::Name,
};
use hvmc::ast::{Net, Tree};

pub fn hvmc_to_net(net: &Net) -> INet {
  let inodes = hvmc_to_inodes(net);
  inodes_to_inet(&inodes)
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
  for (i, (tree1, tree2)) in net.redexes.iter().enumerate() {
    let tree_root = format!("a{i}");
    let mut tree1 = tree_to_inodes(tree1, tree_root.clone(), net_root, &mut n_vars);
    inodes.append(&mut tree1);
    let mut tree2 = tree_to_inodes(tree2, tree_root, net_root, &mut n_vars);
    inodes.append(&mut tree2);
  }
  inodes
}

fn new_var(n_vars: &mut NodeId) -> String {
  let new_var = format!("x{n_vars}");
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
    if let Tree::Var { nam } = subtree {
      return if nam == net_root { "_".to_string() } else { nam.clone() };
    }
    if let Tree::Ctr { ports, .. } = subtree {
      if ports.len() == 1 {
        return process_node_subtree(&ports[0], net_root, subtrees, n_vars);
      }
    }

    let var = new_var(n_vars);
    subtrees.push((var.clone(), subtree));
    var
  }

  fn process_ctr<'a>(
    inodes: &mut Vec<INode>,
    lab: u16,
    ports: &'a [Tree],
    net_root: &str,
    principal: String,
    subtrees: &mut Vec<(String, &'a Tree)>,
    n_vars: &mut NodeId,
  ) {
    fn process_sub_ctr<'a>(
      inodes: &mut Vec<INode>,
      lab: u16,
      ports: &'a [Tree],
      net_root: &str,
      subtrees: &mut Vec<(String, &'a Tree)>,
      n_vars: &mut NodeId,
    ) -> String {
      if ports.len() == 1 {
        process_node_subtree(&ports[0], net_root, subtrees, n_vars)
      } else {
        let principal = new_var(n_vars);
        process_ctr(inodes, lab, ports, net_root, principal.clone(), subtrees, n_vars);
        principal
      }
    }
    if ports.is_empty() {
      let inner = new_var(n_vars);
      inodes.push(INode { kind: Era, ports: [principal, inner.clone(), inner] });
    } else if ports.len() == 1 {
      subtrees.push((principal, &ports[0]));
    } else {
      // build a 2-ary node
      let kind = NodeKind::Ctr(CtrKind::from_lab(lab));
      let rgt = process_sub_ctr(inodes, lab, &ports[1 ..], net_root, subtrees, n_vars);
      let lft = process_node_subtree(&ports[0], net_root, subtrees, n_vars);
      inodes.push(INode { kind, ports: [principal.clone(), lft.clone(), rgt.clone()] });
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
      Tree::Ctr { lab, ports } => {
        process_ctr(&mut inodes, *lab, ports, net_root, subtree_root, &mut subtrees, n_vars)
      }
      Tree::Var { .. } => unreachable!(),
      Tree::Ref { nam } => {
        let kind = Ref { def_name: Name::new(nam) };
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Num { val } => {
        let kind = Num { val: *val };
        let var = new_var(n_vars);
        inodes.push(INode { kind, ports: [subtree_root, var.clone(), var] });
      }
      Tree::Op { fst, snd } => {
        let kind = NodeKind::Op2;
        let fst = process_node_subtree(fst, net_root, &mut subtrees, n_vars);
        let snd = process_node_subtree(snd, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, fst, snd] });
      }
      Tree::Mat { zero, succ, out } => {
        let kind = Mat;
        let zero = process_node_subtree(zero, net_root, &mut subtrees, n_vars);
        let succ = process_node_subtree(succ, net_root, &mut subtrees, n_vars);
        let sel_var = new_var(n_vars);
        inodes.push(INode { kind: NodeKind::Ctr(CtrKind::Con(None)), ports: [sel_var.clone(), zero, succ] });
        let ret = process_node_subtree(out, net_root, &mut subtrees, n_vars);
        inodes.push(INode { kind, ports: [subtree_root, sel_var, ret] });
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
