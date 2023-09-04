#![feature(slice_group_by)]

use clap::Parser;
use hvm2::lang::show_net;
use std::{path::PathBuf, time::Instant};

pub mod ast;
pub mod loader;
pub mod parser;
pub mod to_core;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[arg(long, default_value = "true", help = "Just checks if the file ok. Default mode", group = "mode")]
  pub check: bool,

  #[arg(short, long, group = "mode", help = "Compiles to HVM-core and prints to stdout")]
  pub compile: bool,

  #[arg(short, long, group = "mode", help = "Runs the hvm-lang file.")]
  pub run: bool,

  #[arg(short, long)]
  pub verbose: bool,

  #[arg(help = "Path to the input file")]
  pub path: PathBuf,
}

fn main() -> anyhow::Result<()> {
  let args = Args::parse();

  let book = loader::load_file_to_book(&args.path)?;

  if args.verbose {
    println!("{book:?}");
  }

  let core_book = to_core::book_to_hvm_core(&book)?;

  if args.compile {
    println!("{core_book}");
  }

  if args.run {
    let (mut root, runtime_book) = to_core::book_to_hvm_internal(core_book);

    let start_time = Instant::now();
    // Computes its normal form
    root.normalize(&runtime_book);

    let elapsed = start_time.elapsed().as_secs_f64();
    // Shows results and stats

    println!("[net]\n{}", show_net(&root));
    println!("size: {}", root.node.len());
    println!("used: {}", root.used);
    let cost = root.rwts;

    let rps = cost as f64 / elapsed / 1_000_000.0;

    println!("Time: {elapsed:.3}s | Rwts: {cost} | RPS: {rps:.3}m");
  }

  Ok(())
}
