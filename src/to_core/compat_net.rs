use crate::ast::{
  compat::{
    addr, enter, kind, link, new_inet, new_node, port, slot, INet, NodeId, NodeKind, Port, CON, DUP, ERA,
    LABEL_MASK, NUM, NUMOP, REF, ROOT, TAG_MASK,
  },
  Name, Term,
};
use hvm_core::{LNet, LTree, Tag};
use std::collections::HashMap;

/// Converts an IC term into an IC net.
pub fn term_to_compat_net(term: &Term) -> anyhow::Result<INet> {
  let mut inet = new_inet();

  // Encodes the main term.
  let main = encode_term(&mut inet, term, ROOT, &mut HashMap::new(), &mut vec![], &mut 0)?;
  if ROOT != main {
    link(&mut inet, ROOT, main);
  }

  Ok(inet)
}

/// Adds a subterm connected to `up` to the `inet`.
/// `scope` has the current variable scope.
/// `vars` has the information of which ports the variables are declared and used in.
/// Expects variables to be affine, refs to be stored as Refs and all names to be bound.
fn encode_term(
  inet: &mut INet,
  term: &Term,
  up: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
  dups: &mut NodeId,
) -> anyhow::Result<Port> {
  match term {
    // A lambda becomes to a con node. Ports:
    // - 0: points to where the lambda occurs.
    // - 1: points to the lambda variable.
    // - 2: points to the lambda body.
    Term::Lam { nam: name, bod: body } => {
      let fun = new_node(inet, CON);

      push_scope(name, port(fun, 1), scope, vars);
      let bod = encode_term(inet, body, port(fun, 2), scope, vars, dups)?;
      pop_scope(name, port(fun, 1), inet, scope);

      link(inet, port(fun, 2), bod);
      Ok(port(fun, 0))
    }
    // An application becomes to a con node too. Ports:
    // - 0: points to the function being applied.
    // - 1: points to the function's argument.
    // - 2: points to where the application occurs.
    Term::App { fun, arg } => {
      let app = new_node(inet, CON);
      let fun = encode_term(inet, fun, port(app, 0), scope, vars, dups)?;
      link(inet, port(app, 0), fun);
      let arg = encode_term(inet, arg, port(app, 1), scope, vars, dups)?;
      link(inet, port(app, 1), arg);
      Ok(port(app, 2))
    }
    // A dup becomes a dup node too. Ports:
    // - 0: points to the value projected.
    // - 1: points to the occurrence of the first variable.
    // - 2: points to the occurrence of the second variable.
    Term::Dup { fst, snd, val, nxt } => {
      let dup = new_node(inet, DUP | *dups);
      *dups += 1;
      let val = encode_term(inet, val, port(dup, 0), scope, vars, dups)?;
      link(inet, val, port(dup, 0));

      push_scope(fst, port(dup, 1), scope, vars);
      push_scope(snd, port(dup, 2), scope, vars);
      let nxt = encode_term(inet, nxt, up, scope, vars, dups)?;
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
      Ok(*declare_port)
    }
    Term::Ref { def_id } => {
      let node = new_node(inet, REF | **def_id);
      link(inet, port(node, 1), port(node, 2));
      link(inet, up, port(node, 0));
      Ok(port(node, 0))
    }
    Term::Num { val } => {
      debug_assert!(**val <= LABEL_MASK);
      let node = new_node(inet, NUM | **val);
      // TODO: This is a workaround with the vector of nodes representation that didn't have number support
      inet.nodes[port(node, 1) as usize] = port(node, 2);
      inet.nodes[port(node, 2) as usize] = port(node, 1);
      Ok(port(node, 0))
    }
    Term::NumOp { op, fst, snd } => {
      let node = new_node(inet, NUMOP | u8::from(*op) as NodeKind);
      let fst = encode_term(inet, fst, port(node, 0), scope, vars, dups)?;
      link(inet, port(node, 0), fst);
      let snd = encode_term(inet, snd, port(node, 1), scope, vars, dups)?;
      link(inet, port(node, 1), snd);
      Ok(port(node, 2))
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

pub fn compat_net_to_core(inet: &INet) -> anyhow::Result<LNet> {
  let (root_root, acts_roots) = get_tree_roots(inet);
  let mut port_to_var_id: HashMap<Port, VarId> = HashMap::new();
  let root = if let Some(root_root) = root_root {
    // If there is a root tree connected to the root node
    compat_tree_to_hvm_tree(inet, root_root, &mut port_to_var_id)
  } else {
    // If the root node points to some aux port (application)
    port_to_var_id.insert(enter(inet, ROOT), 0);
    LTree::Var { nam: var_id_to_name(0) }
  };
  let mut acts = vec![];
  for [root0, root1] in acts_roots {
    let act0 = compat_tree_to_hvm_tree(inet, root0, &mut port_to_var_id);
    let act1 = compat_tree_to_hvm_tree(inet, root1, &mut port_to_var_id);
    acts.push((act0, act1));
  }
  Ok(LNet { root, acts })
}

type VarId = NodeId;

/// Returns a list of all the tree node roots in the compat inet.
fn get_tree_roots(inet: &INet) -> (Option<NodeId>, Vec<[NodeId; 2]>) {
  let mut acts_roots: Vec<[NodeId; 2]> = vec![];
  let mut explored_nodes = vec![false; inet.nodes.len() / 4];
  let mut side_links: Vec<Port> = vec![]; // Links between trees

  // Start by checking the root tree (if any)
  explored_nodes[addr(ROOT) as usize] = true;
  let root_link = enter(inet, ROOT);
  let root_root = if slot(root_link) == 0 {
    // If the root node is connected to a main port, we have a root tree
    let root_node = addr(root_link);
    go_down_tree(inet, root_node, &mut explored_nodes, &mut side_links);
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
      let new_roots = go_up_tree(inet, dest_node);
      go_down_tree(inet, new_roots[0], &mut explored_nodes, &mut side_links);
      go_down_tree(inet, new_roots[1], &mut explored_nodes, &mut side_links);
      acts_roots.push(new_roots);
    }
  }

  (root_root, acts_roots)
}

/// Go down a node tree, marking all nodes with the tree_id and storing any side_links found.
fn go_down_tree(inet: &INet, root: NodeId, explored_nodes: &mut [bool], side_links: &mut Vec<Port>) {
  debug_assert!(!explored_nodes[root as usize], "Explored same tree twice");
  let mut nodes_to_check = vec![root];
  while let Some(node) = nodes_to_check.pop() {
    debug_assert!(!explored_nodes[node as usize]);
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
}

/// Goes up a node tree, starting from some given node.
/// Returns the root of this tree and the root of its active pair.
fn go_up_tree(inet: &INet, start_node: NodeId) -> [NodeId; 2] {
  let mut crnt_node = start_node;
  loop {
    let up_port = enter(inet, port(crnt_node, 0));
    let up_node = addr(up_port);
    if slot(up_port) == 0 {
      return [crnt_node, up_node];
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
    REF => LTree::Ref { nam: label },
    NUM => LTree::NUM { val: label },
    NUMOP => todo!(),
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
      LTree::Var { nam: var_id_to_name(var_id) }
    } else {
      // New var
      let var_id = port_to_var_id.len() as VarId;
      port_to_var_id.insert(dst_port, var_id);
      LTree::Var { nam: var_id_to_name(var_id) }
    }
  }
}

fn var_id_to_name(mut var_id: VarId) -> String {
  let mut name = String::new();
  loop {
    let c = (var_id % 26) as u8 + b'a';
    name.push(c as char);
    var_id /= 26;
    if var_id == 0 {
      break;
    }
  }
  name
  // format!("x{var_id}")
}
