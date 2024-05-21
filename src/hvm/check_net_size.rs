use super::ast::Book;
use crate::{diagnostics::Diagnostics, fun::Name};

pub const MAX_NET_SIZE: usize = 64;

pub fn check_net_sizes(book: &Book, diagnostics: &mut Diagnostics) -> Result<(), Diagnostics> {
  diagnostics.start_pass();

  for (name, net) in &book.nets {
    let nodes = count_nodes(net);
    if nodes > MAX_NET_SIZE {
      diagnostics.add_rule_error(
        format!("Definition is too large for hvm (size={nodes}, max size={MAX_NET_SIZE}). Please break it into smaller pieces."),
        Name::new(name),
      );
    }
  }

  diagnostics.fatal(())
}

/// Utility function to count the amount of nodes in an hvm-core AST net
pub fn count_nodes<'l>(net: &'l super::ast::Net) -> usize {
  let mut visit: Vec<&'l super::ast::Tree> = vec![&net.root];
  let mut count = 0usize;
  for (_, l, r) in &net.redexes {
    visit.push(l);
    visit.push(r);
  }
  while let Some(tree) = visit.pop() {
    // If it is not 0-ary, then we'll count it as a node.
    if tree.children().next().is_some() {
      count += 1;
    }
    for subtree in tree.children() {
      visit.push(subtree);
    }
  }
  count
}
