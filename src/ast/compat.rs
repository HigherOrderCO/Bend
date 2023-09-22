// TODO: Refactor to not use this intermediate form

use hvm_core::{Val, OP};

#[derive(Clone, Debug)]
/// Net representation used only as an intermediate for converting to hvm-core format
pub struct INet {
  pub nodes: Vec<Val>,
}

pub type NodeKind = Val;
pub type Port = Val;
pub type NodeId = Val;
pub type SlotId = Val;

/// The ROOT port is on the deadlocked root node at address 0.
pub const ROOT: Port = 1;
pub const TAG_WIDTH: u32 = 16; // TODO: Make this generic over the HVM type.
pub const TAG: u32 = Val::BITS - TAG_WIDTH;
pub const ERA: NodeKind = 0 << TAG;
pub const CON: NodeKind = 1 << TAG;
pub const DUP: NodeKind = 2 << TAG;
pub const REF: NodeKind = 3 << TAG;
pub const NUM_U32: NodeKind = 4 << TAG;
pub const NUM_I32: NodeKind = 5 << TAG;
pub const NUMOP: NodeKind = 6 << TAG;
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

/// Kind of the node.
pub fn kind(inet: &INet, node: NodeId) -> NodeKind {
  inet.nodes[port(node, 3) as usize]
}

/// Links two ports.
pub fn link(inet: &mut INet, ptr_a: Port, ptr_b: Port) {
  inet.nodes[ptr_a as usize] = ptr_b;
  inet.nodes[ptr_b as usize] = ptr_a;
}

pub fn op_to_label(value: OP) -> NodeKind {
  match value {
    OP::ADD => 0x0,
    OP::SUB => 0x1,
    OP::MUL => 0x2,
    OP::DIV => 0x3,
    OP::MOD => 0x4,
    OP::EQ => 0x5,
    OP::NEQ => 0x6,
    OP::LT => 0x7,
    OP::GT => 0x8,
    OP::LTE => 0x9,
    OP::GTE => 0xa,
    OP::AND => 0xb,
    OP::OR => 0xc,
  }
}

pub fn label_to_op(value: NodeKind) -> OP {
  match value {
    0x0 => OP::ADD,
    0x1 => OP::SUB,
    0x2 => OP::MUL,
    0x3 => OP::DIV,
    0x4 => OP::MOD,
    0x5 => OP::EQ,
    0x6 => OP::NEQ,
    0x7 => OP::LT,
    0x8 => OP::GT,
    0x9 => OP::LTE,
    0xa => OP::GTE,
    0xb => OP::AND,
    0xc => OP::OR,
    _ => unreachable!(),
  }
}
#[derive(Debug)]
pub struct INode {
  pub kind: NodeKind,
  pub ports: [String; 3],
}

pub type INodes = Vec<INode>;
