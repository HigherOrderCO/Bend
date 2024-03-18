use std::sync::{Arc, Mutex};

use hvmc::{
  host::Host,
  run::{Def, LabSet, Mode, Net, Port, Tag, Wire},
  stdlib::{ArcDef, AsArcDef, HostedDef},
};

use crate::builtins::util::AsDefFunction;

use super::util::FunctionLikeHosted;

pub(crate) fn add_exit_def(host: Arc<Mutex<Host>>) {
  /// `HVM.exit`.
  /// Implements the following reduction rule
  ///
  /// ```txt
  /// ExitDef ~ (a b)
  /// ---
  /// ExitDefGetStatus ~ a & <dangling> ~ b
  /// ```
  struct ExitDef;

  impl AsDefFunction for ExitDef {
    fn call<M: Mode>(&self, net: &mut Net<M>, input: Wire, output: Wire) {
      // Purposefully deadlock "output" to prevent further reduction
      drop(output);

      net.link_wire_port(input, ArcDef::new_arc_port(LabSet::ALL, ExitDefGetStatus));
    }
  }

  /// Def that scans for a number, and exits the program with that number as the exit code
  ///
  /// ```txt
  /// ExitDefGetStatus ~ {lab a b} // also works on match and op
  /// ---
  /// ExitDefGetStatus ~ a & ExitDefGetStatus ~ b
  ///
  /// ExitDefGetStatus ~ #num
  /// ---
  /// process::exit(num)
  ///
  /// ExitDefGetStatus ~ other
  /// ---
  /// process::exit(-1)
  /// ```
  struct ExitDefGetStatus;
  impl AsArcDef for ExitDefGetStatus {
    fn call<M: Mode>(slf: Arc<Def<Self>>, net: &mut Net<M>, port: Port) {
      match port.tag() {
        Tag::Red | Tag::Var | Tag::Num => {
          std::process::exit(port.num().try_into().unwrap_or(-1));
        }
        Tag::Ref => {
          std::process::exit(-1);
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

  host
    .lock()
    .unwrap()
    .insert_def("HVM.exit", unsafe { HostedDef::new_hosted(LabSet::ALL, FunctionLikeHosted(ExitDef)) });
}
