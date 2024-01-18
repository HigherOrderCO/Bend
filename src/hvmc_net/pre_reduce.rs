// Reduce the compiled networks, solving any annihilations and commutations.
// This is a useful optimization on its own, but also required by an hvm-core optimization.

use crate::{
  expand, net_from_runtime, net_to_runtime, rdex, reduce, runtime_net_to_runtime_def, term::DefNames,
};
use hvmc::{
  ast::{book_from_runtime, name_to_val, Book},
  run::{Net, Ptr, OP2, REF},
};

const MAX_ITERS: usize = 100_000;

/// Reduces the definitions in the book individually, except for main.
/// If cross_refs, will deref and try to find the smallest net.
/// Otherwise, just apply node~node interactions.
pub fn pre_reduce_book(book: &mut Book, cross_refs: bool) -> Result<(), String> {
  let rt_book = &mut hvmc::ast::book_to_runtime(book);
  for (nam, net) in book.iter() {
    // Skip unnecessary work
    if net.rdex.is_empty() {
      continue;
    }
    let mut rt = &mut Net::new(1 << 18, false);
    let fid = name_to_val(nam);
    boot(rt, fid);
    expand(rt, rt_book);

    let fully_reduce = cross_refs && nam != DefNames::ENTRY_POINT;
    let iters = if fully_reduce {
      let mut iters = 0;
      // TODO: If I just call `rt.normal` some terms expand infinitely, so I put this workaround.
      // But I don't think this is the right way (even if it works).
      loop {
        iters += reduce(rt, &rt_book, MAX_ITERS);
        if heap_root(rt).is_ref() {
          expand(rt, rt_book);
        } else {
          break;
        }
      }
      iters
    } else {
      reduce_without_deref(&mut rt, MAX_ITERS)
    };
    if iters > MAX_ITERS {
      return Err(format!("Unable to pre-reduce definition {nam} in under {MAX_ITERS} iterations"));
    }

    let new_def = runtime_net_sparse_to_runtime_def(&rt);

    let def = rt_book.defs.get_mut(&fid).unwrap();
    def.rdex = new_def.rdex;
    def.node = new_def.node;
  }
  *book = book_from_runtime(rt_book);
  Ok(())
}

/// Normalizes a runtime net, except for derefs.
fn reduce_without_deref(net: &mut Net, limit: usize) -> usize {
  let book = hvmc::run::Book::new();
  let mut collected_redexes = vec![];
  let mut iters = 0;

  // Separate the derefing interactions, reduce all the others.
  // Also, always put the Ref on the left side (first element).
  // The mem::replace thing implements a queue using 2 vecs.
  let mut rdexes = std::mem::take(rdex(net));
  while !rdexes.is_empty() {
    for (a, b) in rdexes {
      match (a.tag(), b.tag()) {
        // But things would start to grow pretty quickly and we need some criteria for which net to choose and when to stop.
        (REF, OP2 ..) | (OP2 .., REF) => collected_redexes.push((a, b)),
        _ => interact(net, &book, a, b),
      }
      if iters >= limit {
        break;
      }
      iters += 1;
    }
    rdexes = std::mem::take(rdex(net));
  }
  // Move the collected redexes back to the net
  *rdex(net) = collected_redexes;

  iters
}

/// Converts a runtime net after execution, with null elements in the heap, into to an hvmc Def.
fn runtime_net_sparse_to_runtime_def(rt_net: &Net) -> hvmc::run::Def {
  // Convert back and forth to compress the net.
  // Hacky, but works and not that slow.
  let net = net_from_runtime(rt_net);
  let mut rt_net = Net::new(1 << 18, false);
  net_to_runtime(&mut rt_net, &net);
  runtime_net_to_runtime_def(&rt_net)
}

fn boot(rt: &mut Net, fid: u64) {
  match rt {
    Net::Eager(net) => net.net.boot(fid),
    _ => unimplemented!(),
  }
}

fn heap_root(rt: &Net) -> Ptr {
  match rt {
    Net::Eager(net) => net.net.heap.get_root(),
    _ => unimplemented!(),
  }
}

fn interact(rt: &mut Net, book: &hvmc::run::Book, a: Ptr, b: Ptr) {
  match rt {
    Net::Eager(net) => net.net.interact(book, a, b),
    _ => unimplemented!(),
  }
}
