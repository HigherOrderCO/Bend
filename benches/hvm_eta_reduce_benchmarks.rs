#![feature(test)]

extern crate test;
mod hvm_common;

use bend::hvm::eta_reduce::eta_reduce_hvm_net;
use hvm_common::{create_test_net, SimpleRng};
use test::Bencher;

#[bench]
fn bench_eta_reduce_small(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let net = create_test_net(&mut rng, 5, 3);
  b.iter(|| eta_reduce_hvm_net(&mut net.clone()));
}
