use crate::ast::{
  compat::{
    addr, enter, kind, label_to_op, link, new_inet, new_node, op_to_label, port, slot, INet, NodeId,
    NodeKind, Port, CON, DUP, ERA, LABEL_MASK, NUMOP, NUM_I32, NUM_U32, REF, ROOT, TAG_MASK,
  },
  var_id_to_name, DefId, Name, Term,
};
use hvm_core::{LNet, LTree, Tag};
use std::collections::{HashMap, HashSet};

/// Converts an IC term into an IC net.
pub fn term_to_compat_net(term: &Term) -> anyhow::Result<INet> {
  let mut inet = new_inet();

  // Encodes the main term.
  let mut global_vars = HashMap::new();
  let main = encode_term(&mut inet, term, ROOT, &mut HashMap::new(), &mut vec![], &mut global_vars, &mut 0)?;

  for (decl_port, use_port) in global_vars.into_values() {
    link(&mut inet, decl_port, use_port);
  }
  if Some(ROOT) != main {
    link_local(&mut inet, ROOT, main);
  }

  Ok(inet)
}

/// Adds a subterm connected to `up` to the `inet`.
/// `scope` has the current variable scope.
/// `vars` has the information of which ports the variables are declared and used in.
/// `global_vars` has the same information for global lambdas. Must be linked outside this function.
/// Expects variables to be affine, refs to be stored as Refs and all names to be bound.
fn encode_term(
  inet: &mut INet,
  term: &Term,
  up: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
  global_vars: &mut HashMap<Name, (Port, Port)>,
  dups: &mut NodeId,
) -> anyhow::Result<Option<Port>> {
  match term {
    // A lambda becomes to a con node. Ports:
    // - 0: points to where the lambda occurs.
    // - 1: points to the lambda variable.
    // - 2: points to the lambda body.
    Term::Lam { nam, bod } => {
      let fun = new_node(inet, CON);
      push_scope(nam, port(fun, 1), scope, vars);
      let bod = encode_term(inet, bod, port(fun, 2), scope, vars, global_vars, dups)?;
      pop_scope(nam, port(fun, 1), inet, scope);
      link_local(inet, port(fun, 2), bod);
      Ok(Some(port(fun, 0)))
    }
    Term::Chn { nam, bod } => {
      let fun = new_node(inet, CON);
      global_vars.entry(nam.clone()).or_default().0 = port(fun, 1);
      let bod = encode_term(inet, bod, port(fun, 2), scope, vars, global_vars, dups)?;
      link_local(inet, port(fun, 2), bod);
      Ok(Some(port(fun, 0)))
    }
    Term::Let { nam, val, nxt } => todo!(),
    // An application becomes to a con node too. Ports:
    // - 0: points to the function being applied.
    // - 1: points to the function's argument.
    // - 2: points to where the application occurs.
    Term::App { fun, arg } => {
      let app = new_node(inet, CON);
      let fun = encode_term(inet, fun, port(app, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(app, 0), fun);
      let arg = encode_term(inet, arg, port(app, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(app, 1), arg);
      Ok(Some(port(app, 2)))
    }
    // A dup becomes a dup node too. Ports:
    // - 0: points to the value projected.
    // - 1: points to the occurrence of the first variable.
    // - 2: points to the occurrence of the second variable.
    Term::Dup { fst, snd, val, nxt } => {
      let dup = new_node(inet, DUP | *dups);
      *dups += 1;
      let val = encode_term(inet, val, port(dup, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(dup, 0), val);

      push_scope(fst, port(dup, 1), scope, vars);
      push_scope(snd, port(dup, 2), scope, vars);
      let nxt = encode_term(inet, nxt, up, scope, vars, global_vars, dups)?;
      pop_scope(snd, port(dup, 2), inet, scope);
      pop_scope(fst, port(dup, 1), inet, scope);

      Ok(nxt)
    }
    Term::Var { nam } => {
      // We assume this variable to be valid, bound and correctly scoped.
      // This pass must be done before.
      debug_assert!(scope.contains_key(nam), "Unbound variable {nam}");
      let var_stack = scope.get(nam).unwrap();
      let crnt_var = *var_stack.last().unwrap();
      let (declare_port, use_port) = vars.get_mut(crnt_var).unwrap();
      debug_assert!(use_port.is_none(), "Variable {nam} used more than once");
      link(inet, up, *declare_port);
      *use_port = Some(up);
      Ok(Some(*declare_port))
    }
    Term::Lnk { nam } => {
      global_vars.entry(nam.clone()).or_default().1 = up;
      Ok(None)
    }
    Term::Ref { def_id } => {
      let node = new_node(inet, REF | **def_id);
      link(inet, port(node, 1), port(node, 2));
      link(inet, up, port(node, 0));
      Ok(Some(port(node, 0)))
    }
    Term::U32 { val } => {
      debug_assert!(*val as NodeKind <= LABEL_MASK);
      let node = new_node(inet, NUM_U32 | *val as NodeKind);
      // TODO: This is a workaround with the vector of nodes representation that didn't have number support
      inet.nodes[port(node, 1) as usize] = port(node, 2);
      inet.nodes[port(node, 2) as usize] = port(node, 1);
      Ok(Some(port(node, 0)))
    }
    Term::I32 { val } => {
      debug_assert!(*val as u32 as NodeKind <= LABEL_MASK);
      let node = new_node(inet, NUM_I32 | *val as u32 as NodeKind);
      // TODO: This is a workaround with the vector of nodes representation that didn't have number support
      inet.nodes[port(node, 1) as usize] = port(node, 2);
      inet.nodes[port(node, 2) as usize] = port(node, 1);
      Ok(Some(port(node, 0)))
    }
    Term::Opx { op, fst, snd } => {
      let node = new_node(inet, NUMOP | op_to_label(*op));
      let fst = encode_term(inet, fst, port(node, 0), scope, vars, global_vars, dups)?;
      link_local(inet, port(node, 0), fst);
      let snd = encode_term(inet, snd, port(node, 1), scope, vars, global_vars, dups)?;
      link_local(inet, port(node, 1), snd);
      Ok(Some(port(node, 2)))
    }
    Term::Sup { .. } => unreachable!(),
    Term::Era => unreachable!(),
  }
}

fn push_scope(
  name: &Option<Name>,
  decl_port: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
) {
  if let Some(name) = name {
    scope.entry(name.clone()).or_default().push(vars.len());
    vars.push((decl_port, None));
  }
}

fn pop_scope(name: &Option<Name>, decl_port: Port, inet: &mut INet, scope: &mut HashMap<Name, Vec<usize>>) {
  if let Some(name) = name {
    scope.get_mut(name).unwrap().pop().unwrap();
  } else {
    let era = new_node(inet, ERA);
    link(inet, decl_port, port(era, 0));
    link(inet, port(era, 1), port(era, 2));
  }
}

fn link_local(inet: &mut INet, ptr_a: Port, ptr_b: Option<Port>) {
  if let Some(ptr_b) = ptr_b {
    link(inet, ptr_a, ptr_b);
  }
}

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

fn compat_tree_to_hvm_tree(inet: &INet, root: NodeId, port_to_var_id: &mut HashMap<Port, VarId>) -> LTree {
  let kind = kind(inet, root);
  let tag = kind & TAG_MASK;
  let label = kind & LABEL_MASK; // TODO: Check if label too high, do something about it.
  match tag {
    ERA => LTree::Era,
    CON => LTree::Nod {
      tag: hvm_core::CON,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    DUP => LTree::Nod {
      tag: hvm_core::DUP + label as Tag,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    REF => LTree::Ref { nam: DefId(label).to_internal() },
    NUM_U32 => LTree::U32 { val: label as u32 },
    NUM_I32 => LTree::I32 { val: label as u32 as i32 },
    NUMOP => LTree::OpX {
      opx: label_to_op(label),
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
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
