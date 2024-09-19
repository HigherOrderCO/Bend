#![feature(test)]

extern crate test;
mod hvm_common;

use bend::diagnostics::Diagnostics;
use bend::hvm::check_net_size::{check_net_sizes, count_nodes};
use bend::CompilerTarget;
use hvm_common::{books_cases, create_test_net, SimpleRng};
use test::Bencher;

#[bench]
fn bench_fuzzing_check_net_sizes_c(b: &mut Bencher) {
  let book = books_cases();
  b.iter(|| {
    let mut diagnostics = Diagnostics::new(Default::default());
    check_net_sizes(&book, &mut diagnostics, &CompilerTarget::C)
  });
}

#[bench]
fn bench_fuzzing_check_net_sizes_cuda(b: &mut Bencher) {
  let book = books_cases();
  b.iter(|| {
    let mut diagnostics = Diagnostics::new(Default::default());
    check_net_sizes(&book, &mut diagnostics, &CompilerTarget::Cuda)
  });
}

#[bench]
fn bench_fuzzing_count_nodes_small(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let net = create_test_net(&mut rng, 5, 15);

  b.iter(|| count_nodes(&net));
}

#[bench]
fn bench_fuzzing_count_nodes_large(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let net = create_test_net(&mut rng, 10, 15);

  b.iter(|| count_nodes(&net));
}
