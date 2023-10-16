pub mod hvmc_to_net;
pub mod net_to_hvmc;

pub use hvmc_to_net::core_net_to_compat;
pub use net_to_hvmc::{compat_net_to_core, nets_to_hvm_core};

use crate::term::{DefId, Op};
use hvmc::run::Val;
use NodeKind::*;

#[derive(Clone, Debug)]
/// Net representation used only as an intermediate for converting to hvm-core format
pub struct INet {
  nodes: Vec<Node>,
}

#[derive(Debug, Clone, Copy)]
pub struct Node {
  pub main: Port,
  pub aux1: Port,
  pub aux2: Port,
  pub kind: NodeKind,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Port(pub NodeId, pub SlotId);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
  Era,
  Con,
  Dup { lab: u8 },
  Ref { def_id: DefId },
  Num { val: Val },
  Op2,
  Mat,
}

pub type NodeId = Val;
pub type SlotId = Val;

/// The ROOT port is on the deadlocked root node at address 0.
pub const ROOT: Port = Port(0, 1);
pub const TAG_WIDTH: u32 = 4; // TODO: Make this generic over the HVM type.
pub const TAG: u32 = Val::BITS - TAG_WIDTH;
pub const LABEL_MASK: Val = (1 << TAG) - 1;
pub const TAG_MASK: Val = !LABEL_MASK;

impl INet {
  /// Create a new net, with a deadlocked root node.
  pub fn new() -> Self {
    INet {
      nodes: vec![Node::new(Port(0, 2), Port(0, 1), Port(0, 0), Era)], // p2 points to p0, p1 points to net
    }
  }

  /// Allocates a new node with its ports disconnected.
  pub fn new_node(&mut self, kind: NodeKind) -> NodeId {
    let idx = self.nodes.len() as NodeId;
    let node = Node::new(Port(idx, 0), Port(idx, 1), Port(idx, 2), kind);
    self.nodes.extend([node]);
    idx
  }

  /// Returns a copy of a node.
  pub fn node(&self, node: NodeId) -> Node {
    self.nodes[node as usize]
  }

  /// Returns the value stored at a port, the port on the other side of the given one.
  pub fn enter_port(&self, port: Port) -> Port {
    self.node(port.node()).port(port.slot())
  }

  /// Links two ports.
  pub fn link(&mut self, a: Port, b: Port) {
    self.set(a, b);
    self.set(b, a);
  }

  /// Sets a port to point to another port
  pub fn set(&mut self, src: Port, dst: Port) {
    *self.nodes[src.node() as usize].port_mut(src.slot()) = dst;
  }
}

impl Node {
  pub fn new(main: Port, aux1: Port, aux2: Port, kind: NodeKind) -> Self {
    Node { main, aux1, aux2, kind }
  }

  pub fn port(&self, slot: SlotId) -> Port {
    match slot {
      0 => self.main,
      1 => self.aux1,
      2 => self.aux2,
      _ => unreachable!(),
    }
  }

  pub fn port_mut(&mut self, slot: SlotId) -> &mut Port {
    match slot {
      0 => &mut self.main,
      1 => &mut self.aux1,
      2 => &mut self.aux2,
      _ => unreachable!(),
    }
  }
}

impl Port {
  /// Returns the node address of a port.
  pub fn node(self) -> NodeId {
    self.0
  }

  /// Returns the slot of a port.
  pub fn slot(self) -> SlotId {
    self.1
  }
}

impl Op {
  pub fn to_hvmc_label(self) -> Val {
    match self {
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

  pub fn from_hvmc_label(value: Val) -> Option<Op> {
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
}

/* INodes representation: */

/// A flat inet representation where links are represented by shared wire names.
// TODO: Find a better name
pub type INodes = Vec<INode>;

#[derive(Debug)]
pub struct INode {
  pub kind: NodeKind,
  pub ports: [String; 3],
}
