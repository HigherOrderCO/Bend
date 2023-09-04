use crate::ast::{
  self,
  core::{LNet, LTree, Tag},
  DefId, Name, Term,
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
/// Net representation used only as an intermediate for converting to hvm-core format
pub struct INet {
  pub nodes: Vec<NodeVal>,
}

type NodeVal = u64;
type NodeKind = NodeVal;
type Port = NodeVal;
type NodeId = NodeVal;
type SlotId = NodeVal;

/// The ROOT port is on the deadlocked root node at address 0.
const ROOT: Port = 1;
const TAG_WIDTH: u32 = Tag::BITS;
const TAG: u32 = 64 - TAG_WIDTH;
const ERA: NodeKind = 0 << TAG;
const CON: NodeKind = 1 << TAG;
const DUP: NodeKind = 2 << TAG;
const REF: NodeKind = 3 << TAG;
const NUM: NodeKind = 4 << TAG;
const NUMOP: NodeKind = 5 << TAG;
const LABEL_MASK: NodeKind = (1 << TAG) - 1;
const TAG_MASK: NodeKind = !LABEL_MASK;

/// Create a new net, with a deadlocked root node.
pub fn new_inet() -> INet {
  INet {
    nodes: vec![2, 1, 0, ERA], // p2 points to p0, p1 points to net
  }
}

/// Allocates a new node, reclaiming a freed space if possible.
pub fn new_node(inet: &mut INet, kind: NodeKind) -> NodeId {
  let node = addr(inet.nodes.len() as Port);
  inet.nodes.extend([port(node, 0), port(node, 1), port(node, 2), kind]);
  node
}

/// Builds a port (an address / slot pair).
pub fn port(node: NodeId, slot: SlotId) -> Port {
  (node << 2) | slot
}

/// Returns the address of a port (TODO: rename).
pub fn addr(port: Port) -> NodeId {
  port >> 2
}

/// Returns the slot of a port.
pub fn slot(port: Port) -> SlotId {
  port & 3
}

/// Enters a port, returning the port on the other side.
pub fn enter(inet: &INet, port: Port) -> Port {
  inet.nodes[port as usize]
}

/// Kind of the node.
pub fn kind(inet: &INet, node: NodeId) -> NodeKind {
  inet.nodes[port(node, 3) as usize]
}

/// Links two ports.
pub fn link(inet: &mut INet, ptr_a: Port, ptr_b: Port) {
  inet.nodes[ptr_a as usize] = ptr_b;
  inet.nodes[ptr_b as usize] = ptr_a;
}

/// Converts an IC term into an IC net.
pub fn term_to_compat_net(term: &Term, def_to_id: &HashMap<Name, DefId>) -> anyhow::Result<INet> {
  let mut inet = new_inet();
  // Initializes state variables
  // For each variable we hold where they are declared and where it's stored
  let mut vars = vec![];
  let mut scope = HashMap::new();

  // Encodes the main term.
  let main = encode_term(&mut inet, term, ROOT, &mut scope, &mut vars, &mut 0, def_to_id)?;

  // Link unused variables
  // TODO: Could be refactored.
  // This could be done directly when exiting the scope of a variable, but code was getting messy.
  for (declare_port, use_port) in vars {
    if use_port.is_none() && enter(&inet, declare_port) == declare_port {
      let era = new_node(&mut inet, ERA);
      link(&mut inet, declare_port, port(era, 0));
      link(&mut inet, port(era, 1), port(era, 2));
    }
  }

  if ROOT != main {
    link(&mut inet, ROOT, main);
  }
  Ok(inet)
}

/// Adds a subterm connected to `up` to the `inet`.
/// `scope` has the current variable scope.
/// `vars` has the information of which ports the variables are declared and used in.
fn encode_term(
  inet: &mut INet,
  term: &Term,
  up: Port,
  scope: &mut HashMap<Name, Vec<usize>>,
  vars: &mut Vec<(Port, Option<Port>)>,
  dups: &mut NodeId,
  def_to_id: &HashMap<Name, DefId>,
) -> anyhow::Result<Port> {
  match term {
    // A lambda becomes to a con node. Ports:
    // - 0: points to where the lambda occurs.
    // - 1: points to the lambda variable.
    // - 2: points to the lambda body.
    Term::Lam { nam: name, bod: body } => {
      let fun = new_node(inet, CON);
      let decl_port = port(fun, 1);

      // TODO: This scoping operation is repeated every time, can refactor.
      scope.entry(name.clone()).or_default().push(vars.len());
      vars.push((decl_port, None));
      let bod = encode_term(inet, body, port(fun, 2), scope, vars, dups, def_to_id)?;
      scope.get_mut(name).unwrap().pop().unwrap();

      link(inet, port(fun, 2), bod);
      Ok(port(fun, 0))
    }
    // An application becomes to a con node too. Ports:
    // - 0: points to the function being applied.
    // - 1: points to the function's argument.
    // - 2: points to where the application occurs.
    Term::App { fun, arg } => {
      let app = new_node(inet, CON);
      let fun = encode_term(inet, fun, port(app, 0), scope, vars, dups, def_to_id)?;
      link(inet, port(app, 0), fun);
      let arg = encode_term(inet, arg, port(app, 1), scope, vars, dups, def_to_id)?;
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
      let val = encode_term(inet, val, port(dup, 0), scope, vars, dups, def_to_id)?;
      link(inet, val, port(dup, 0));

      scope.entry(fst.clone()).or_default().push(vars.len());
      vars.push((port(dup, 1), None));
      scope.entry(snd.clone()).or_default().push(vars.len());
      vars.push((port(dup, 2), None));
      let nxt = encode_term(inet, nxt, up, scope, vars, dups, def_to_id)?;
      scope.get_mut(snd).unwrap().pop().unwrap();
      scope.get_mut(fst).unwrap().pop().unwrap();
      Ok(nxt)
    }
    Term::Var { nam } => {
      // Try to mark this as a used variable
      if let Some(var_stack) = scope.get(nam) {
        if let Some(&crnt_var) = var_stack.last() {
          let (declare_port, use_port) = vars.get_mut(crnt_var).unwrap();
          if use_port.is_none() {
            link(inet, up, *declare_port);
            *use_port = Some(up);
            return Ok(*declare_port); // Variable use ok, return early
          } else {
            // This has to be checked earlier
            return Err(anyhow::anyhow!("Variable used more than once: '{}'", nam.as_ref()));
          }
        }
      }
      // If the name is not in scope, check if it's a definition's name
      if let Some(definition_id) = def_to_id.get(nam) {
        let node = new_node(inet, REF | **definition_id);
        link(inet, port(node, 1), port(node, 2));
        link(inet, up, port(node, 0));
        Ok(port(node, 0))
      } else {
        // This has to be checked earlier
        Err(anyhow::anyhow!("Unbound variable: '{}'", nam.as_ref()))
      }
    }
    Term::Num { val } => {
      let node = new_node(inet, NUM);
      // TODO: This is a workaround with the vector of nodes representation that didn't have number support
      inet.nodes[port(node, 1) as usize] = **val;
      inet.nodes[port(node, 2) as usize] = **val;
      Ok(port(node, 0))
    }
    Term::NumOp { op, fst, snd } => {
      let node = new_node(inet, NUMOP | u8::from(*op) as NodeKind);
      let fst = encode_term(inet, fst, port(node, 0), scope, vars, dups, def_to_id)?;
      link(inet, port(node, 0), fst);
      let snd = encode_term(inet, snd, port(node, 1), scope, vars, dups, def_to_id)?;
      link(inet, port(node, 1), snd);
      Ok(port(node, 2))
    }
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
      tag: ast::core::CON,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    DUP => LTree::Nod {
      tag: ast::core::DUP + label as Tag,
      lft: Box::new(var_or_subtree(inet, port(root, 1), port_to_var_id)),
      rgt: Box::new(var_or_subtree(inet, port(root, 2), port_to_var_id)),
    },
    REF => LTree::Ref { nam: label },
    NUM => LTree::NUM { val: enter(inet, port(root, 1)) },
    NUMOP => todo!(), // TODO: HVM2 doesn't have numeric operator atm.
    _ => unreachable!("{tag:x}"),
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

fn var_id_to_name(var_id: VarId) -> String {
  format!("x{var_id}")
}
