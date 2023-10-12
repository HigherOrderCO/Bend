use super::inter_net::*;
use crate::term::*;
use hvmc::{LNet, LTree, Tag};
use std::collections::{HashMap, HashSet};

pub fn compat_net_to_core(inet: &INet) -> anyhow::Result<LNet> {
  let (root_root, redx_roots) = get_tree_roots(inet)?;
  let mut port_to_var_id: HashMap<Port, VarId> = HashMap::new();
  let root = if let Some(root_root) = root_root {
    // If there is a root tree connected to the root node
    compat_tree_to_hvm_tree(inet, root_root, &mut port_to_var_id)
  } else {
    // If the root node points to some aux port (application)
    port_to_var_id.insert(enter(inet, ROOT), 0);
    LTree::Var { nam: var_id_to_name(0).0 }
  };
  let mut rdex = vec![];
  for [root0, root1] in redx_roots {
    let rdex0 = compat_tree_to_hvm_tree(inet, root0, &mut port_to_var_id);
    let rdex1 = compat_tree_to_hvm_tree(inet, root1, &mut port_to_var_id);
    rdex.push((rdex0, rdex1));
  }
  Ok(LNet { root, rdex })
}

fn compat_tree_to_hvm_tree(inet: &INet, root: NodeId, port_to_var_id: &mut HashMap<Port, VarId>) -> LTree {
  let kind = kind(inet, root);
  let tag = kind & TAG_MASK;
  let label = kind & LABEL_MASK; // TODO: Check if label too high, do something about it.
  match tag {
    ERA => LTree::Era,
    CON => LTree::Ctr {
      lab: 0,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    DUP => LTree::Ctr {
      lab: (label + 1) as Tag,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    REF => LTree::Ref { nam: DefId(label).to_internal() },
    NUM => LTree::Num { val: label as u32 },
    OP2 => LTree::Op2 {
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    ITE => LTree::Ite {
      sel: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      ret: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    _ => unreachable!("Invalid tag in compat tree {tag:x}"),
  }
}

fn var_or_subtree(inet: &INet, src_port: Port, port_to_var_id: &mut HashMap<Port, VarId>) -> LTree {
  let dst_port = enter(inet, src_port);
  if slot(dst_port) == 0 {
    // Subtree
    compat_tree_to_hvm_tree(inet, addr(dst_port), port_to_var_id)
  } else {
    // Var
    if let Some(&var_id) = port_to_var_id.get(&src_port) {
      // Previously found var
      LTree::Var { nam: var_id_to_name(var_id).0 }
    } else {
      // New var
      let var_id = port_to_var_id.len() as VarId;
      port_to_var_id.insert(dst_port, var_id);
      LTree::Var { nam: var_id_to_name(var_id).0 }
    }
  }
}

type VarId = NodeId;

/// Returns a list of all the tree node roots in the compat inet.
fn get_tree_roots(inet: &INet) -> anyhow::Result<(Option<NodeId>, Vec<[NodeId; 2]>)> {
  let mut redx_roots: Vec<[NodeId; 2]> = vec![];
  let mut explored_nodes = vec![false; inet.nodes.len() / 4];
  let mut side_links: Vec<Port> = vec![]; // Links between trees

  // Start by checking the root tree (if any)
  explored_nodes[addr(ROOT) as usize] = true;
  let root_link = enter(inet, ROOT);
  let root_root = if slot(root_link) == 0 {
    // If the root node is connected to a main port, we have a root tree
    let root_node = addr(root_link);
    go_down_tree(inet, root_node, &mut explored_nodes, &mut side_links)?;
    Some(root_node)
  } else {
    // Otherwise, root node connected to an aux port, no root tree.
    side_links.push(root_link);
    None
  };

  // Check each side-link for a possible new tree pair;
  while let Some(dest_port) = side_links.pop() {
    let dest_node = addr(dest_port);
    // Only go up unmarked trees
    if !explored_nodes[dest_node as usize] {
      let new_roots = go_up_tree(inet, dest_node)?;
      go_down_tree(inet, new_roots[0], &mut explored_nodes, &mut side_links)?;
      go_down_tree(inet, new_roots[1], &mut explored_nodes, &mut side_links)?;
      redx_roots.push(new_roots);
    }
  }

  Ok((root_root, redx_roots))
}

/// Go down a node tree, marking all nodes with the tree_id and storing any side_links found.
fn go_down_tree(
  inet: &INet,
  root: NodeId,
  explored_nodes: &mut [bool],
  side_links: &mut Vec<Port>,
) -> anyhow::Result<()> {
  debug_assert!(!explored_nodes[root as usize], "Explored same tree twice");
  let mut nodes_to_check = vec![root];
  while let Some(node) = nodes_to_check.pop() {
    if explored_nodes[node as usize] {
      return Err(anyhow::anyhow!("Cyclic terms are not supported"));
    }
    explored_nodes[node as usize] = true;
    for down_slot in [1, 2] {
      let down_port = enter(inet, port(node, down_slot));
      if slot(down_port) == 0 {
        // If this down-link is to a main port, this is a node of the same tree
        nodes_to_check.push(addr(down_port));
      } else {
        // Otherwise it's a side-link
        side_links.push(down_port);
      }
    }
  }
  Ok(())
}

/// Goes up a node tree, starting from some given node.
/// Returns the root of this tree and the root of its active pair.
fn go_up_tree(inet: &INet, start_node: NodeId) -> anyhow::Result<[NodeId; 2]> {
  let mut explored_nodes = HashSet::new();
  let mut crnt_node = start_node;
  loop {
    if !explored_nodes.insert(crnt_node) {
      return Err(anyhow::anyhow!("Cyclic terms are not supported"));
    }
    let up_port = enter(inet, port(crnt_node, 0));
    let up_node = addr(up_port);
    if slot(up_port) == 0 {
      return Ok([crnt_node, up_node]);
    } else {
      crnt_node = up_node;
    }
  }
}
