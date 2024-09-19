use hvm::ast::{Book, Net, Tree};
use std::collections::BTreeMap;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct SimpleRng(u64);

impl SimpleRng {
  pub fn new() -> Self {
    SimpleRng(SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards").as_secs())
  }

  pub fn gen_range(&mut self, low: u64, high: u64) -> u64 {
    self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1);
    low + (self.0 >> 32) % (high - low)
  }
}

impl Default for SimpleRng {
  fn default() -> Self {
    Self::new()
  }
}

// Fuzzing testing by create tree
pub fn create_random_tree(rng: &mut SimpleRng, depth: u32, var_prefix: &str) -> Tree {
  if depth == 0 || rng.gen_range(0, 100) < 30 {
    // 30% chance to generate a Var or Era
    if rng.gen_range(0, 100) < 50 {
      Tree::Var { nam: format!("{}_{}", var_prefix, rng.gen_range(0, 10)) }
    } else {
      Tree::Era
    }
  } else {
    // 70% chance to generate a Con or Dup
    let is_con = rng.gen_range(0, 100) < 50;
    let fst = Box::new(create_random_tree(rng, depth - 1, var_prefix));
    let snd = Box::new(create_random_tree(rng, depth - 1, var_prefix));

    if is_con {
      Tree::Con { fst, snd }
    } else {
      Tree::Dup { fst, snd }
    }
  }
}
// Fuzzing testing by create book
#[allow(dead_code)]
pub fn create_test_book(rng: &mut SimpleRng, num_defs: usize, max_depth: u32, max_rbag: usize) -> Book {
  let mut defs = BTreeMap::new();
  for i in 0..num_defs {
    let name = format!("Def_{}", i);
    let depth = (rng.gen_range(1, max_depth as u64 + 1)) as u32;
    let root = create_random_tree(rng, depth, "root");
    let rbag_size = rng.gen_range(0, max_rbag as u64 + 1) as usize;
    let rbag = (0..rbag_size)
      .map(|_| {
        (
          false,
          create_random_tree(rng, 20, &format!("left_{}", i)),
          create_random_tree(rng, 20, &format!("right_{}", i)),
        )
      })
      .collect();
    defs.insert(name, Net { root, rbag });
  }
  for i in 0..num_defs / 10 {
    let name = format!("Inline_{}", i);
    let root = Tree::Ref { nam: format!("Ref_{}", rng.gen_range(0, num_defs as u64)) };
    defs.insert(name, Net { root, rbag: vec![] });
  }
  Book { defs }
}
// Fuzzing testing by create net
#[allow(dead_code)]
pub fn books_cases() -> Book {
  let mut rng = SimpleRng::new();
  create_test_book(&mut rng, 100, 5, 3)
}

// Fuzzing testing by create net
#[allow(dead_code)]
pub fn create_test_net(rng: &mut SimpleRng, max_refs: u32, size: u32) -> Net {
  let root = create_random_tree(rng, max_refs, "root");
  let mut rbag = Vec::new();
  for i in 0..size {
    let left = create_random_tree(rng, size, &format!("left_{}", i));
    let right = create_random_tree(rng, size, &format!("right_{}", i));
    rbag.push((false, left, right));
  }

  Net { root, rbag }
}
