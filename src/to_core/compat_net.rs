use std::collections::HashMap;

use crate::ast::{DefId, Name, Term};

#[derive(Clone, Debug)]
/// Net representation used only as an intermediate for converting to hvm-core format
pub struct INet {
  pub nodes: Vec<u32>,
}

pub type NodeKind = u32;

/// A port is just a u32 combining address (30 bits) and slot (2 bits).
pub type Port = u32;
pub type NodeId = u32;
pub type SlotId = u32;

/// The ROOT port is on the deadlocked root node at address 0.
pub const ROOT: Port = 1;
pub const TAG: NodeKind = 28;
pub const ERA: NodeKind = 0 << TAG;
pub const CON: NodeKind = 1 << TAG;
pub const DUP: NodeKind = 2 << TAG;
pub const REF: NodeKind = 3 << TAG;
pub const NUM: NodeKind = 4 << TAG;
pub const NUMOP: NodeKind = 5 << TAG;
pub const LABEL_MASK: NodeKind = (1 << TAG) - 1;
pub const TAG_MASK: NodeKind = !LABEL_MASK;

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

/// Enters a slot on the node pointed by this port.
pub fn get(inet: &INet, p: Port, s: SlotId) -> Port {
  enter(inet, port(addr(p), s))
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
  dups: &mut u32,
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
      let dup = new_node(inet, DUP + *dups);
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
        let node = new_node(inet, REF + **definition_id);
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
      let node = new_node(inet, NUMOP + u8::from(*op) as u32);
      let fst = encode_term(inet, fst, port(node, 0), scope, vars, dups, def_to_id)?;
      link(inet, port(node, 0), fst);
      let snd = encode_term(inet, snd, port(node, 1), scope, vars, dups, def_to_id)?;
      link(inet, port(node, 1), snd);
      Ok(port(node, 2))
    }
  }
}
