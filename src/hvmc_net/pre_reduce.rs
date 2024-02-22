// Reduce the compiled networks, solving any annihilations and commutations.
// This is a useful optimization on its own, but also required by an hvm-core optimization.

use std::sync::Mutex;

use hvmc::{ast::Book, dispatch_dyn_net, host::Host, run::Def};

use crate::CORE_BUILTINS;
use hvmc::run::Port;

/// A Def that pushes all interactions to its inner Vec.
struct InertDef(Mutex<Vec<(Port, Port)>>);

impl hvmc::run::AsDef for InertDef {
  unsafe fn call<M: hvmc::run::Mode>(
    def: *const hvmc::run::Def<Self>,
    _: &mut hvmc::run::Net<M>,
    port: Port,
  ) {
    let def = unsafe { &*def };
    def.data.0.lock().unwrap().push((Port::new_ref(def), port));
  }
}

/// Reduces the definitions in the book individually, except for main.
///
/// It does not reduce interactions that use builtin defs, as they are
/// assumed to be side-effectful
pub fn pre_reduce_book(book: &mut Book, entrypoint: &str) -> Result<(), String> {
  /// Maximum amount of rewrites that
  const MAX_RWTS: usize = 100_000;
  // Create a host
  // with inert definitions in the place
  // of core builtins, to prevent them from being reduced
  let mut host = Host::default();
  for builtin in CORE_BUILTINS {
    let def = InertDef(Default::default());
    host.insert_def(builtin, hvmc::host::DefRef::Owned(Box::new(Def::new(hvmc::run::LabSet::ALL, def))));
  }
  host.insert_book(book);

  for (nam, net) in book.iter_mut() {
    // Skip unnecessary work
    if net.rdex.is_empty() || *nam == entrypoint {
      continue;
    }

    let area = hvmc::run::Net::<hvmc::run::Lazy>::init_heap(1 << 18);
    let mut rt = hvmc::run::DynNet::new(&area, false);
    dispatch_dyn_net!(&mut rt => {
      rt.boot(host.defs.get(nam).expect("No function."));
      rt.expand();
      rt.reduce(MAX_RWTS);
    });

    // Move interactions with inert defs back into the net rdex array
    for def in host.defs.values() {
      if let Some(def) = def.downcast_ref::<&Def<InertDef>>() {
        let mut stored_redexes = def.data.data.0.lock().unwrap();
        dispatch_dyn_net!(&mut rt => {
          rt.rdex.extend(core::mem::take(&mut *stored_redexes));
        })
      }
    }
    // Place the reduced net back into the def map
    dispatch_dyn_net!(&mut rt => {
      *net = host.readback(&rt);
    });
  }
  Ok(())
}
