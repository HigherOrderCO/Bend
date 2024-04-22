use hvmc::ast::Net;

use crate::diagnostics::Diagnostics;

use super::{term_to_net::Labels, Book, Term};

mod linear;
mod non_linear;

pub fn readback(net: &Net, book: &Book, labels: &Labels, linear: bool, info: &mut Diagnostics) -> Term {
  if linear {
    linear::readback_linear(net, book, labels, info)
  } else {
    non_linear::readback_non_linear(net, book, labels, info)
  }
}
