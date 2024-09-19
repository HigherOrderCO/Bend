#![feature(test)]

extern crate test;

mod hvm_common;
use bend::hvm::{hvm_book_show_pretty, net_trees, net_trees_mut, tree_children, tree_children_mut};
use hvm_common::{books_cases, create_random_tree, create_test_net, SimpleRng};
use test::Bencher;

#[bench]
fn bench_fuzzing_tree_children(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let tree = create_random_tree(&mut rng, 10, "root");

  b.iter(|| {
    for child in tree_children(&tree) {
      test::black_box(child);
    }
  });
}

#[bench]
fn bench_fuzzing_tree_children_mut(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let mut tree = create_random_tree(&mut rng, 10, "root");

  b.iter(|| {
    for child in tree_children_mut(&mut tree) {
      test::black_box(child);
    }
  });
}

#[bench]
fn bench_fuzzing_net_trees(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let net = create_test_net(&mut rng, 5, 3);

  b.iter(|| {
    for tree in net_trees(&net) {
      test::black_box(tree);
    }
  });
}
// original https://github.com/HigherOrderCO/Bend/blob/main/src/hvm/mod.rs#L39
#[allow(dead_code)]
fn original_hvm_book_show_pretty(book: &hvm::ast::Book) -> String {
  let mut s = String::new();
  for (nam, def) in book.defs.iter() {
    s.push_str(&format!("@{} = {}\n", nam, def.root.show()));
    for (pri, a, b) in def.rbag.iter() {
      s.push_str("  &");
      if *pri {
        s.push('!');
      } else {
        s.push(' ');
      }
      s.push_str(&a.show());
      s.push_str(" ~ ");
      s.push_str(&b.show());
      s.push('\n');
    }
    s.push('\n');
  }
  s
}

#[bench]
fn bench_fuzzing_net_trees_mut(b: &mut Bencher) {
  let mut rng = SimpleRng::new();
  let mut net = create_test_net(&mut rng, 5, 3);

  b.iter(|| {
    for tree in net_trees_mut(&mut net) {
      test::black_box(tree);
    }
  });
}

#[bench]
fn bench_original(b: &mut Bencher) {
  let book = books_cases();
  b.iter(|| {
    test::black_box(original_hvm_book_show_pretty(&book));
  });
}

#[bench]
fn bench_optimized(b: &mut Bencher) {
  let book = books_cases();
  b.iter(|| {
    test::black_box(hvm_book_show_pretty(&book));
  });
}
