#![feature(test)]

extern crate test;
mod hvm_common;

use bend::hvm::inline::inline_hvm_book;
use hvm_common::{create_test_book, SimpleRng};
use test::Bencher;

#[bench]
fn bench_fuzzing_inline_small(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let mut test_book = create_test_book(&mut rng, 50, 5, 10);
  b.iter(|| inline_hvm_book(&mut test_book));
}

#[bench]
fn bench_fuzzing_inline_medium(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let mut test_book = create_test_book(&mut rng, 100, 10, 10);
  b.iter(|| inline_hvm_book(&mut test_book));
}

#[bench]
fn bench_fuzzing_inline_large(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let mut test_book = create_test_book(&mut rng, 500, 15, 10);
  b.iter(|| inline_hvm_book(&mut test_book));
}
