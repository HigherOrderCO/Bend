use crate::{diagnostics::Diagnostics, term::Name};
use hvmc::ast::Book;

pub const MAX_NET_SIZE: usize = 32;

pub fn check_net_sizes(book: &Book, diagnostics: &mut Diagnostics) -> Result<(), Diagnostics> {
  diagnostics.start_pass();

  for (name, net) in &book.nets {
    if count_nodes(net) > MAX_NET_SIZE {
      diagnostics.add_rule_error(
        "Definition is too large for hvm. Please break it into smaller pieces.",
        Name::new(name),
      );
    }
  }

  diagnostics.fatal(())
}

/// Utility function to count the amount of nodes in an hvm-core AST net
pub fn count_nodes<'l>(net: &'l hvmc::ast::Net) -> usize {
  let mut visit: Vec<&'l hvmc::ast::Tree> = vec![&net.root];
  let mut count = 0usize;
  for (l, r) in &net.redexes {
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
