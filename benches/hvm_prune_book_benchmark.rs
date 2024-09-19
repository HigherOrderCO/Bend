#![feature(test)]
extern crate test;
mod hvm_common;

use bend::hvm::prune::prune_hvm_book;
use hvm::ast::{Book, Net, Tree};
use hvm_common::SimpleRng;
use std::collections::BTreeMap;
use test::Bencher;

fn create_test_book(rng: &mut SimpleRng, num_defs: usize, max_depth: u32, max_rbag: usize) -> Book {
  let mut defs = BTreeMap::new();
  for i in 0..num_defs {
    let name = format!("Def_{}", i);
    let depth = (rng.gen_range(1, max_depth as u64 + 1)) as u32;
    let root = create_random_tree(rng, depth, num_defs);
    let rbag_size = rng.gen_range(0, max_rbag as u64 + 1) as usize;
    let rbag = (0..rbag_size)
      .map(|_| (false, create_random_tree(rng, 2, num_defs), create_random_tree(rng, 2, num_defs)))
      .collect();
    defs.insert(name, Net { root, rbag });
  }

  // Ensure some definitions are inlineable
  for i in 0..num_defs / 10 {
    let name = format!("Inline_{}", i);
    let root = Tree::Ref { nam: format!("Ref_{}", rng.gen_range(0, num_defs as u64)) };
    defs.insert(name, Net { root, rbag: vec![] });
  }

  Book { defs }
}

fn create_random_tree(rng: &mut SimpleRng, depth: u32, max_refs: usize) -> Tree {
  if depth == 0 || rng.gen_range(0, 100) < 30 {
    // 30% chance to generate a Ref or Era
    if rng.gen_range(0, 100) < 50 {
      Tree::Ref { nam: format!("Ref_{}", rng.gen_range(0, max_refs as u64)) }
    } else {
      Tree::Era
    }
  } else {
    // 70% chance to generate a Con or Dup
    let is_con = rng.gen_range(0, 100) < 50;
    let fst = Box::new(create_random_tree(rng, depth - 1, max_refs));
    let snd = Box::new(create_random_tree(rng, depth - 1, max_refs));
    if is_con {
      Tree::Con { fst, snd }
    } else {
      Tree::Dup { fst, snd }
    }
  }
}

#[bench]
fn bench_prune_hvm_book(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let entrypoints = vec!["Def_0".to_string(), "Def_50".to_string()];
  b.iter(|| {
    let mut book = create_test_book(&mut rng, 50, 5, 10); // Book with 50 definitions
    prune_hvm_book(&mut book, &entrypoints);
  });
}
