use super::{INet, NodeId, NodeKind, Port, ROOT};

use crate::term::num_to_name;
use hvmc::ast::{Book, Net, Tree};
use std::collections::{HashMap, HashSet};

/// Converts the inet-encoded definitions into an hvmc AST Book.
pub fn nets_to_hvmc(nets: HashMap<String, INet>) -> Result<Book, String> {
  let mut book = Book::default();
  for (name, inet) in nets {
    let net = net_to_hvmc(&inet)?;
    book.insert(name, net);
  }
  Ok(book)
}

/// Convert an inet-encoded definition into an hvmc AST inet.
pub fn net_to_hvmc(inet: &INet) -> Result<Net, String> {
  let (net_root, redexes) = get_tree_roots(inet)?;
  let mut port_to_var_id: HashMap<Port, VarId> = HashMap::new();
  let root = if let Some(net_root) = net_root {
    // If there is a root tree connected to the root node
    net_tree_to_hvmc_tree(inet, net_root, &mut port_to_var_id)
  } else {
    // If the root node points to some aux port (application)
    port_to_var_id.insert(inet.enter_port(ROOT), 0);
    Tree::Var { nam: num_to_name(0) }
  };
  let mut rdex = vec![];
  for [root0, root1] in redexes {
    let rdex0 = net_tree_to_hvmc_tree(inet, root0, &mut port_to_var_id);
    let rdex1 = net_tree_to_hvmc_tree(inet, root1, &mut port_to_var_id);
    rdex.push((rdex0, rdex1));
  }
  Ok(Net { root, rdex })
}

fn net_tree_to_hvmc_tree(inet: &INet, tree_root: NodeId, port_to_var_id: &mut HashMap<Port, VarId>) -> Tree {
  match &inet.node(tree_root).kind {
    NodeKind::Era => Tree::Era,
    NodeKind::Con { lab: None } => Tree::Ctr {
      lab: 0,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Tup => Tree::Ctr {
      lab: 1,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Con { lab: Some(lab) } => Tree::Ctr {
      #[allow(clippy::identity_op)]
      lab: (*lab as u16 + 1) << 1 | 0,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Dup { lab } => Tree::Ctr {
      lab: (*lab as u16 + 1) << 1 | 1,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Ref { def_name } => Tree::Ref { nam: def_name.to_string() },
    NodeKind::Num { val } => Tree::Num { val: *val },
    NodeKind::Op2 { opr } => Tree::Op2 {
      opr: *opr,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Mat => Tree::Mat {
      sel: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id)),
      ret: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id)),
    },
    NodeKind::Rot => unreachable!(),
  }
}

fn var_or_subtree(inet: &INet, src_port: Port, port_to_var_id: &mut HashMap<Port, VarId>) -> Tree {
  let dst_port = inet.enter_port(src_port);
  if dst_port.slot() == 0 {
    // Subtree
    net_tree_to_hvmc_tree(inet, dst_port.node(), port_to_var_id)
  } else {
    // Var
    if let Some(&var_id) = port_to_var_id.get(&src_port) {
      // Previously found var
      Tree::Var { nam: num_to_name(var_id) }
    } else {
      // New var
      let var_id = port_to_var_id.len() as VarId;
      port_to_var_id.insert(dst_port, var_id);
      Tree::Var { nam: num_to_name(var_id) }
    }
  }
}

type VarId = NodeId;

/// Finds the roots of all the trees in the inet.
/// Returns them as the root of the root tree and the active pairs of the net.
/// Active pairs are found by a right-to-left, depth-first search.
fn get_tree_roots(inet: &INet) -> Result<(Option<NodeId>, Vec<[NodeId; 2]>), String> {
  let mut redex_roots: Vec<[NodeId; 2]> = vec![];
  let mut movements: Vec<Movement> = vec![];
  let mut root_set = HashSet::from([ROOT.node()]);
  let mut explored_nodes = vec![false; inet.nodes.len()];

  // Start by checking the root tree (if any)
  explored_nodes[ROOT.node() as usize] = true;
  let root_link = inet.enter_port(ROOT);
  let root_node = root_link.node();
  let root_tree_root = if root_link.slot() == 0 {
    // If the root node is connected to a main port, we have a root tree
    movements.push(Movement::Down(root_node));
    root_set.insert(root_node);
    Some(root_node)
  } else {
    // Otherwise, root node connected to an aux port, no root tree.
    movements.push(Movement::Side(root_node));
    None
  };

  // Traverse the net
  while let Some(movement) = movements.pop() {
    match movement {
      Movement::Down(node_id) => explore_down_link(inet, node_id, &mut explored_nodes, &mut movements),
      Movement::Side(node_id) => {
        explore_side_link(inet, node_id, &mut movements, &mut redex_roots, &mut root_set)?;
      }
    }
  }

  Ok((root_tree_root, redex_roots))
}

enum Movement {
  Down(NodeId),
  Side(NodeId),
}

fn explore_down_link(
  inet: &INet,
  node_id: NodeId,
  explored_nodes: &mut [bool],
  movements: &mut Vec<Movement>,
) {
  // Don't go down already explored nodes.
  if !explored_nodes[node_id as usize] {
    explored_nodes[node_id as usize] = true;
    for down_slot in [1, 2] {
      let down_port = inet.enter_port(Port(node_id, down_slot));
      let movement = if down_port.slot() == 0 || down_port == ROOT {
        // If this down-link is to a main port, this is a node of the same tree
        Movement::Down(down_port.node())
      } else {
        // Otherwise it's a side-link
        Movement::Side(down_port.node())
      };
      movements.push(movement);
    }
  }
}

fn explore_side_link(
  inet: &INet,
  node_id: NodeId,
  movements: &mut Vec<Movement>,
  redex_roots: &mut Vec<[NodeId; 2]>,
  root_set: &mut HashSet<NodeId>,
) -> Result<(), String> {
  let new_roots = go_up_tree(inet, node_id)?;
  // If this is a new tree, explore it downwards
  if !root_set.contains(&new_roots[0]) && !root_set.contains(&new_roots[1]) {
    movements.push(Movement::Down(new_roots[0]));
    movements.push(Movement::Down(new_roots[1]));
    redex_roots.push(new_roots);
    root_set.insert(new_roots[0]);
    root_set.insert(new_roots[1]);
  }
  Ok(())
}

/// Goes up a node tree, starting from some given node.
/// Returns the active pair at the root of this tree.
fn go_up_tree(inet: &INet, start_node: NodeId) -> Result<[NodeId; 2], String> {
  let mut explored_nodes = HashSet::new();
  let mut cur_node = start_node;
  loop {
    if !explored_nodes.insert(cur_node) {
      return Err("Found term that compiles into an inet with a vicious cycle".to_string());
    }

    let up = inet.enter_port(Port(cur_node, 0));

    if up.slot() == 0 || up == ROOT {
      return Ok([up.node(), cur_node]);
    }

    cur_node = up.node();
  }
}
