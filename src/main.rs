#![feature(slice_group_by)]

use clap::Parser;
use hvm2::{
  core::{Book, Net},
  lang::{define, name_to_u64, show_net, u64_to_name},
};
use std::{path::PathBuf, time::Instant};

pub mod ast;
pub mod loader;
pub mod parser;
pub mod to_core;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  path: PathBuf,
}

// fn book_to_book

fn main() -> anyhow::Result<()> {
  let args = Args::parse();
  let book = loader::load_file_to_book(&args.path)?;
  // println!("{book:?}");
  let compiled = to_core::book_to_hvm_core(&book)?;
  println!("{compiled}");

  let mut def = Book::new();

  let _def_book = compiled.defs.iter().for_each(|(name, term)| {
    {
      define(&mut def, (u64_to_name(**name as u64)).as_str(), term.to_string().as_str());
    }
  });

  let net = &mut Net::new(1 << 26);

  net.init(name_to_u64("Main"));

  let start_time = Instant::now();
  // Computes its normal form
  net.normalize(&def);

  let elapsed = start_time.elapsed().as_secs_f64();
  // Shows results and stats

  println!("[net]\n{}", show_net(&net));
  println!("size: {}", net.node.len());
  println!("used: {}", net.used);
  let cost = net.rwts;

  let rps = cost as f64 / elapsed / 1_000_000.0;

  println!("Time: {elapsed:.3}s | Rwts: {cost} | RPS: {rps:.3}m");

  Ok(())
}
