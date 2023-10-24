use super::{INet, NodeId, NodeKind, Port, ROOT, BASE_DUP_HVMC_LABEL};
use crate::term::{var_id_to_name, DefId};
use hvmc::{
  ast::{Book, Net, Tree},
  run::{Tag, Val},
};
use std::collections::{HashMap, HashSet};

pub fn nets_to_hvm_core(
  nets: HashMap<String, INet>,
  id_to_hvmc_name: &HashMap<DefId, Val>,
) -> anyhow::Result<Book> {
  let mut book = Book::new();
  for (name, inet) in nets {
    book.insert(name, compat_net_to_core(&inet, &|id| id_to_hvmc_name[&id])?);
  }
  Ok(book)
}

pub fn compat_net_to_core(inet: &INet, id_to_hvmc_name: &impl Fn(DefId) -> Val) -> anyhow::Result<Net> {
  let (net_root, redxs) = get_tree_roots(inet)?;
  let mut port_to_var_id: HashMap<Port, VarId> = HashMap::new();
  let root = if let Some(net_root) = net_root {
    // If there is a root tree connected to the root node
    net_tree_to_hvmc_tree(inet, net_root, &mut port_to_var_id, id_to_hvmc_name)
  } else {
    // If the root node points to some aux port (application)
    port_to_var_id.insert(inet.enter_port(ROOT), 0);
    Tree::Var { nam: var_id_to_name(0).0 }
  };
  let mut rdex = vec![];
  for [root0, root1] in redxs {
    let rdex0 = net_tree_to_hvmc_tree(inet, root0, &mut port_to_var_id, id_to_hvmc_name);
    let rdex1 = net_tree_to_hvmc_tree(inet, root1, &mut port_to_var_id, id_to_hvmc_name);
    rdex.push((rdex0, rdex1));
  }
  Ok(Net { root, rdex })
}

fn net_tree_to_hvmc_tree(
  inet: &INet,
  tree_root: NodeId,
  port_to_var_id: &mut HashMap<Port, VarId>,
  id_to_hvmc_name: &impl Fn(DefId) -> Val,
) -> Tree {
  match inet.node(tree_root).kind {
    NodeKind::Era => Tree::Era,
    NodeKind::Con => Tree::Ctr {
      lab: 0,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, id_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, id_to_hvmc_name)),
    },
    NodeKind::Tup => Tree::Ctr {
      lab: 1,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, id_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, id_to_hvmc_name)),
    },
    NodeKind::Dup { lab } => Tree::Ctr {
      lab: (lab + BASE_DUP_HVMC_LABEL) as Tag,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, id_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, id_to_hvmc_name)),
    },
    NodeKind::Ref { def_id } => Tree::Ref { nam: id_to_hvmc_name(def_id) },
    NodeKind::Num { val } => Tree::Num { val },
    NodeKind::Op2 => Tree::Op2 {
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, id_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, id_to_hvmc_name)),
    },
    NodeKind::Mat => Tree::Mat {
      sel: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, id_to_hvmc_name)),
      ret: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, id_to_hvmc_name)),
    },
    NodeKind::Rot => unreachable!(),
  }
}

fn var_or_subtree(
  inet: &INet,
  src_port: Port,
  port_to_var_id: &mut HashMap<Port, VarId>,
  id_to_hvmc_name: &impl Fn(DefId) -> Val,
) -> Tree {
  let dst_port = inet.enter_port(src_port);
  if dst_port.slot() == 0 {
    // Subtree
    net_tree_to_hvmc_tree(inet, dst_port.node(), port_to_var_id, id_to_hvmc_name)
  } else {
    // Var
    if let Some(&var_id) = port_to_var_id.get(&src_port) {
      // Previously found var
      Tree::Var { nam: var_id_to_name(var_id).0 }
    } else {
      // New var
      let var_id = port_to_var_id.len() as VarId;
      port_to_var_id.insert(dst_port, var_id);
      Tree::Var { nam: var_id_to_name(var_id).0 }
    }
  }
}

type VarId = NodeId;

/// Returns a list of all the tree node roots in the compat inet.
fn get_tree_roots(inet: &INet) -> anyhow::Result<(Option<NodeId>, Vec<[NodeId; 2]>)> {
  let mut redx_roots: Vec<[NodeId; 2]> = vec![];
  let mut explored_nodes = vec![false; inet.nodes.len()];
  let mut side_links: Vec<Port> = vec![]; // Links between trees

  // Start by checking the root tree (if any)
  explored_nodes[ROOT.node() as usize] = true;
  let root_link = inet.enter_port(ROOT);
  let root_root = if root_link.slot() == 0 {
    // If the root node is connected to a main port, we have a root tree
    let root_node = root_link.node();
    go_down_tree(inet, root_node, &mut explored_nodes, &mut side_links)?;
    Some(root_node)
  } else {
    // Otherwise, root node connected to an aux port, no root tree.
    side_links.push(root_link);
    None
  };

  // Check each side-link for a possible new tree pair;
  while let Some(dest_port) = side_links.pop() {
    let dest_node = dest_port.node();
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
  while let Some(node_id) = nodes_to_check.pop() {
    if explored_nodes[node_id as usize] {
      return Err(anyhow::anyhow!("Cyclic terms are not supported"));
    }
    explored_nodes[node_id as usize] = true;
    for down_slot in [1, 2] {
      let down_port = inet.enter_port(Port(node_id, down_slot));
      if down_port.slot() == 0 {
        // If this down-link is to a main port, this is a node of the same tree
        nodes_to_check.push(down_port.node());
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
    let up = inet.enter_port(Port(crnt_node, 0));
    if up.slot() == 0 {
      return Ok([crnt_node, up.node()]);
    } else {
      crnt_node = up.node();
    }
  }
}
