// TODO: Use a proper node type

use crate::term::Op;
use hvmc::Val;

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
pub const TAG_WIDTH: u32 = 4; // TODO: Make this generic over the HVM type.
pub const TAG: u32 = Val::BITS - TAG_WIDTH;
pub const ERA: NodeKind = 0 << TAG;
pub const CON: NodeKind = 1 << TAG;
pub const DUP: NodeKind = 2 << TAG;
pub const REF: NodeKind = 3 << TAG;
pub const NUM: NodeKind = 4 << TAG;
pub const OP2: NodeKind = 6 << TAG;
pub const MAT: NodeKind = 7 << TAG;
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

/// Returns the address of a port.
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

#[derive(Debug)]
pub struct INode {
  pub kind: NodeKind,
  pub ports: [String; 3],
}

pub type INodes = Vec<INode>;

pub fn op_to_label(value: Op) -> NodeKind {
  match value {
    Op::ADD => 0x1,
    Op::SUB => 0x2,
    Op::MUL => 0x3,
    Op::DIV => 0x4,
    Op::MOD => 0x5,
    Op::EQ => 0x6,
    Op::NE => 0x7,
    Op::LT => 0x8,
    Op::GT => 0x9,
    Op::AND => 0xa,
    Op::OR => 0xb,
    Op::XOR => 0xc,
    Op::NOT => 0xd,
    Op::LSH => 0xe,
    Op::RSH => 0xf,
  }
}

pub fn label_to_op(value: NodeKind) -> Option<Op> {
  match value {
    0x1 => Some(Op::ADD),
    0x2 => Some(Op::SUB),
    0x3 => Some(Op::MUL),
    0x4 => Some(Op::DIV),
    0x5 => Some(Op::MOD),
    0x6 => Some(Op::EQ),
    0x7 => Some(Op::NE),
    0x8 => Some(Op::LT),
    0x9 => Some(Op::GT),
    0xa => Some(Op::AND),
    0xb => Some(Op::OR),
    0xc => Some(Op::XOR),
    0xd => Some(Op::NOT),
    0xe => Some(Op::LSH),
    0xf => Some(Op::RSH),
    _ => None,
  }
}
