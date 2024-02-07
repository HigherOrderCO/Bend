use crate::{
  net::{INet, NodeId, NodeKind, Port, ROOT},
  term::{num_to_name, term_to_net::HvmcNames, DefName},
};
use hvmc::{
  ast::{Book, Net, Tree},
  run::Val,
};
use std::collections::{HashMap, HashSet};

/// Converts the inet-encoded definitions into an hvmc AST Book.
pub fn nets_to_hvmc(nets: HashMap<String, INet>, hvmc_names: &HvmcNames) -> Result<Book, String> {
  let mut book = Book::new();
  for (name, inet) in nets {
    let net = net_to_hvmc(&inet, &hvmc_names.hvml_to_hvmc)?;
    book.insert(name, net);
  }
  Ok(book)
}

/// Convert an inet-encoded definition into an hvmc AST inet.
pub fn net_to_hvmc(
  inet: &INet,
  hvml_to_hvmc_name: &HashMap<DefName, Val>,
) -> Result<Net, String> {
  let (net_root, redxs) = get_tree_roots(inet)?;
  let mut port_to_var_id: HashMap<Port, VarId> = HashMap::new();
  let root = if let Some(net_root) = net_root {
    // If there is a root tree connected to the root node
    net_tree_to_hvmc_tree(inet, net_root, &mut port_to_var_id, hvml_to_hvmc_name)
  } else {
    // If the root node points to some aux port (application)
    port_to_var_id.insert(inet.enter_port(ROOT), 0);
    Tree::Var { nam: num_to_name(0) }
  };
  let mut rdex = vec![];
  for [root0, root1] in redxs {
    let rdex0 = net_tree_to_hvmc_tree(inet, root0, &mut port_to_var_id, hvml_to_hvmc_name);
    let rdex1 = net_tree_to_hvmc_tree(inet, root1, &mut port_to_var_id, hvml_to_hvmc_name);
    rdex.push((rdex0, rdex1));
  }
  Ok(Net { root, rdex })
}

fn net_tree_to_hvmc_tree(
  inet: &INet,
  tree_root: NodeId,
  port_to_var_id: &mut HashMap<Port, VarId>,
  hvml_to_hvmc_name: &HashMap<DefName, Val>,
) -> Tree {
  match &inet.node(tree_root).kind {
    NodeKind::Era => Tree::Era,
    NodeKind::Con { lab: None } => Tree::Con {
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Tup => Tree::Tup {
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Con { lab: Some(lab) } => Tree::Dup {
      #[allow(clippy::identity_op)]
      // label shifted left with bit 0 set as 0
      lab: (lab + 1) << 1 | 0,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Dup { lab } => Tree::Dup {
      // label shifted left with bit 0 set as 1
      lab: (lab + 1) << 1 | 1,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Ref { def_name } => Tree::Ref { nam: hvml_to_hvmc_name[def_name] },
    NodeKind::Num { val } => Tree::Num { val: *val },
    NodeKind::Op2 { opr } => Tree::Op2 {
      opr: *opr,
      lft: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      rgt: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Mat => Tree::Mat {
      sel: Box::new(var_or_subtree(inet, Port(tree_root, 1), port_to_var_id, hvml_to_hvmc_name)),
      ret: Box::new(var_or_subtree(inet, Port(tree_root, 2), port_to_var_id, hvml_to_hvmc_name)),
    },
    NodeKind::Rot => unreachable!(),
  }
}

fn var_or_subtree(
  inet: &INet,
  src_port: Port,
  port_to_var_id: &mut HashMap<Port, VarId>,
  hvml_to_hvmc_name: &HashMap<DefName, Val>,
) -> Tree {
  let dst_port = inet.enter_port(src_port);
  if dst_port.slot() == 0 {
    // Subtree
    net_tree_to_hvmc_tree(inet, dst_port.node(), port_to_var_id, hvml_to_hvmc_name)
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
  let mut redx_roots: Vec<[NodeId; 2]> = vec![];
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
        explore_side_link(inet, node_id, &mut movements, &mut redx_roots, &mut root_set)?;
      }
    }
  }

  Ok((root_tree_root, redx_roots))
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
  redx_roots: &mut Vec<[NodeId; 2]>,
  root_set: &mut HashSet<NodeId>,
) -> Result<(), String> {
  let new_roots = go_up_tree(inet, node_id)?;
  // If this is a new tree, explore it downwards
  if !root_set.contains(&new_roots[0]) && !root_set.contains(&new_roots[1]) {
    movements.push(Movement::Down(new_roots[0]));
    movements.push(Movement::Down(new_roots[1]));
    redx_roots.push(new_roots);
    root_set.insert(new_roots[0]);
    root_set.insert(new_roots[1]);
  }
  Ok(())
}

/// Goes up a node tree, starting from some given node.
/// Returns the active pair at the root of this tree.
fn go_up_tree(inet: &INet, start_node: NodeId) -> Result<[NodeId; 2], String> {
  let mut explored_nodes = HashSet::new();
  let mut crnt_node = start_node;
  loop {
    if !explored_nodes.insert(crnt_node) {
      return Err("Found term that compiles into an inet with a vicious cycle".to_string());
    }

    let up = inet.enter_port(Port(crnt_node, 0));

    if up.slot() == 0 || up == ROOT {
      return Ok([up.node(), crnt_node]);
    }

    crnt_node = up.node();
  }
}
