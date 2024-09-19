#![feature(test)]

extern crate test;
mod hvm_common;

use bend::diagnostics::Diagnostics;
use bend::diagnostics::DiagnosticsConfig;
use bend::hvm::mutual_recursion::check_cycles;
use hvm_common::{create_test_book, SimpleRng};

use test::Bencher;

#[bench]
fn bench_check_cycles(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let book = create_test_book(&mut rng, 500, 10, 50);
  b.iter(|| {
    let config = DiagnosticsConfig::default();
    let mut diagnostics = Diagnostics::new(config);
    check_cycles(&book, &mut diagnostics)
  });
}
