use hvmc::{
  run::{Port, Wire},
  stdlib::{ArcDef, AsArcDef, AsHostedDef},
};

/// This utility struct implements `AsArcDef`
/// It is a wrapper around a type, and makes it act like a function.
pub(crate) struct FunctionLike<T: AsDefFunction>(pub T);

pub trait AsDefFunction: Send + Sync + 'static {
  fn call<M: hvmc::run::Mode>(&self, net: &mut hvmc::run::Net<M>, input: Wire, output: Wire);
}

impl<F: AsDefFunction> AsArcDef for FunctionLike<F> {
  fn call<M: hvmc::run::Mode>(
    slf: std::sync::Arc<hvmc::run::Def<Self>>,
    net: &mut hvmc::run::Net<M>,
    port: hvmc::run::Port,
  ) {
    match port.tag() {
      hvmc::run::Tag::Red | hvmc::run::Tag::Var | hvmc::run::Tag::Num | hvmc::run::Tag::Ref => (),
      hvmc::run::Tag::Ctr if port.lab() == 0 => {
        let node = port.consume_node();
        slf.data.0.call(net, node.p1, node.p2);
      }
      _ => {
        // Commute
        let node = port.consume_node();
        net.link_wire_port(node.p1, ArcDef::to_port(slf.clone()));
        net.link_wire_port(node.p2, ArcDef::to_port(slf));
      }
    }
  }
}

pub(crate) struct FunctionLikeHosted<T: AsDefFunction>(pub T);

impl<F: AsDefFunction> AsHostedDef for FunctionLikeHosted<F> {
  fn call<M: hvmc::run::Mode>(
    slf: &hvmc::run::Def<Self>,
    net: &mut hvmc::run::Net<M>,
    port: hvmc::run::Port,
  ) {
    match port.tag() {
      hvmc::run::Tag::Red | hvmc::run::Tag::Var | hvmc::run::Tag::Num | hvmc::run::Tag::Ref => (),
      hvmc::run::Tag::Ctr if port.lab() == 0 => {
        let node = port.consume_node();
        slf.data.0.call(net, node.p1, node.p2);
      }
      _ => {
        let node = port.consume_node();
        net.link_wire_port(node.p1, Port::new_ref(slf));
        net.link_wire_port(node.p2, Port::new_ref(slf));
      }
    }
  }
}
