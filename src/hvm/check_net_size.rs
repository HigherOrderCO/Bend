use super::tree_children;
use crate::{diagnostics::Diagnostics, fun::Name, CompileOpts};
use hvm::ast::{Book, Net, Tree};

pub const MAX_NET_SIZE_C: usize = 4095;
pub const MAX_NET_SIZE_CUDA: usize = 64;

pub fn check_net_sizes(
  book: &Book,
  diagnostics: &mut Diagnostics,
  opts: &CompileOpts,
) -> Result<(), Diagnostics> {
  let net_size_bound = match opts.command.as_str() {
    "run-cu" | "gen-cu" | "gen-hvm" => MAX_NET_SIZE_CUDA,
    _ => MAX_NET_SIZE_C,
  };

  diagnostics.start_pass();

  for (name, net) in &book.defs {
    let nodes = count_nodes(net);
    if nodes > net_size_bound {
      diagnostics.add_rule_error(
        format!("Definition is too large for hvm (size={nodes}, max size={net_size_bound}). Please break it into smaller pieces."),
        Name::new(name),
      );
    }
  }

  diagnostics.fatal(())
}

/// Utility function to count the amount of nodes in an hvm-core AST net
pub fn count_nodes(net: &Net) -> usize {
  let mut visit: Vec<&Tree> = vec![&net.root];
  let mut count = 0usize;
  for (_, l, r) in &net.rbag {
    visit.push(l);
    visit.push(r);
  }
  while let Some(tree) = visit.pop() {
    // If it is not 0-ary, then we'll count it as a node.
    if tree_children(tree).next().is_some() {
      count += 1;
    }
    for subtree in tree_children(tree) {
      visit.push(subtree);
    }
  }
  count
}
