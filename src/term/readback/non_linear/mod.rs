use hvmc::ast::Net;

use crate::{
  diagnostics::Diagnostics,
  term::{encoding::Labels, Book, Term},
};

mod hvmc_to_inet;
mod inet;
mod inet_to_term;

use hvmc_to_inet::*;
use inet::*;
use inet_to_term::*;

pub fn readback_non_linear(net: &Net, book: &Book, labels: &Labels, info: &mut Diagnostics) -> Term {
  let inet = hvmc_to_inet(net);
  inet_to_term(&inet, book, labels, info)
}
