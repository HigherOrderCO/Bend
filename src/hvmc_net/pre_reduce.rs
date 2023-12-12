// Reduce the compiled networks, solving any annihilations and commutations.
// This is a useful optimization on its own, but also required by an hvm-core optimization.

use crate::term::DefNames;
use hvmc::{
  ast::{name_to_val, net_from_runtime, net_to_runtime, runtime_net_to_runtime_def},
  run::{APtr, Heap, OP2, REF, ROOT},
};
use std::collections::BTreeMap;

const MAX_ITERS: usize = 10_000;

/// Reduces the definitions in the book individually, except for main.
/// If cross_refs, will deref and try to find the smallest net.
/// Otherwise, just apply node~node interactions.
pub fn pre_reduce_book(book: &mut BTreeMap<String, hvmc::ast::Net>, cross_refs: bool) -> Result<(), String> {
  let mut rt_book = hvmc::ast::book_to_runtime(book);
  let heap = Heap::init(1 << 18);
  for (nam, net) in book.iter_mut() {
    pre_reduce_net(nam, net, &rt_book, &heap, cross_refs)?;
    let mut rt = hvmc::run::Net::new(&heap);
    net_to_runtime(&mut rt, net);
    rt_book.def(name_to_val(nam), runtime_net_to_runtime_def(&rt));
  }
  Ok(())
}

fn pre_reduce_net(
  nam: &str,
  net: &mut hvmc::ast::Net,
  book: &hvmc::run::Book,
  heap: &[(APtr, APtr)],
  cross_refs: bool,
) -> Result<(), String> {
  let mut rt = hvmc::run::Net::new(heap);
  net_to_runtime(&mut rt, net);

  let mut num_iters = 0;

  reduce_single_run_net(nam, &mut rt, &mut num_iters)?;

  let mut best_net = net_from_runtime(&rt);
  let mut best_net_len = net_len(&best_net);

  // Don't prereduce main, since that is just running the program.
  if cross_refs && nam != DefNames::ENTRY_POINT {
    while deref_once(&mut rt, book) {
      if reduce_single_run_net(nam, &mut rt, &mut num_iters).is_err() {
        break;
      }
      let crnt_net = net_from_runtime(&rt);
      let crnt_net_len = net_len(&crnt_net);
      if crnt_net_len <= best_net_len {
        best_net = crnt_net;
        best_net_len = crnt_net_len;
      }
    }
  }

  *net = best_net;
  Ok(())
}

/// Fully normalizes a runtime net, except for derefs.
fn reduce_single_run_net(nam: &str, net: &mut hvmc::run::Net, num_iters: &mut usize) -> Result<(), String> {
  // Note: not calling Book::new() since that takes super long (~600ms).
  // We know it's okay in this specific case since we are sure no derefs will occur.
  let book = hvmc::run::Book::new();
  let mut collected_redexes = vec![];

  // Separate the derefing interactions, reduce all the others.
  // Also, always put the Ref on the left side (first element).
  // The mem::replace thing implements a queue using 2 vecs.
  let mut rdexes = std::mem::take(&mut net.rdex);
  while !rdexes.is_empty() {
    for (a, b) in rdexes {
      match (a.tag(), b.tag()) {
        // But things would start to grow pretty quickly and we need some criteria for which net to choose and when to stop.
        (REF, OP2 ..) | (OP2 .., REF) => collected_redexes.push((a, b)),
        _ => net.interact(&book, a, b),
      }

      if *num_iters >= MAX_ITERS {
        return Err(format!(
          "Unable to reduce definition '{}' to the required hvmc form in {} reductions",
          nam, MAX_ITERS
        ));
      }
      *num_iters += 1;
    }
    rdexes = std::mem::take(&mut net.rdex);
  }

  // Move the collected redexes back to the net
  net.rdex = collected_redexes;

  Ok(())
}

/// Returns how many nodes in the network.
fn net_len(net: &hvmc::ast::Net) -> usize {
  fn tree_len(tree: &hvmc::ast::Tree) -> usize {
    let mut len = 0;
    let mut trees = vec![tree];
    while let Some(tree) = trees.pop() {
      match tree {
        hvmc::ast::Tree::Con { lft, rgt }
        | hvmc::ast::Tree::Tup { lft, rgt }
        | hvmc::ast::Tree::Dup { lft, rgt, .. }
        | hvmc::ast::Tree::Op2 { lft, rgt, .. }
        | hvmc::ast::Tree::Mat { sel: lft, ret: rgt } => {
          len += 1;
          trees.push(lft);
          trees.push(rgt);
        }
        hvmc::ast::Tree::Op1 { rgt, .. } => {
          len += 1;
          trees.push(rgt);
        }
        hvmc::ast::Tree::Era | hvmc::ast::Tree::Ref { .. } | hvmc::ast::Tree::Num { .. } => {
          len += 1;
        }
        hvmc::ast::Tree::Var { .. } => (),
      }
    }
    len
  }
  let root_len = tree_len(&net.root);
  let rdex_len: usize = net.rdex.iter().map(|(a, b)| tree_len(a) + tree_len(b)).sum();
  root_len + rdex_len
}

/// Reduces a single deref. Returns true if there was a ref to deref.
fn deref_once(net: &mut hvmc::run::Net, book: &hvmc::run::Book) -> bool {
  let mut rdex = None;
  for (i, (a, b)) in net.rdex.iter().enumerate() {
    if a.is_ref() && b.is_nod() || a.is_nod() && b.is_ref() {
      rdex = Some(i);
      break;
    }
  }

  if let Some(rdex) = rdex {
    // Reduce first redex with ref
    let (a, b) = net.rdex.remove(rdex);
    net.interact(book, a, b);
    true
  } else {
    let root = net.get_target(ROOT);
    if root.is_ref() {
      // Expand ref at root
      net.call(book, root, ROOT);
      true
    } else {
      // Nothing to do, normalized
      false
    }
  }
}
