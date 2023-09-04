#![feature(slice_group_by)]

use clap::Parser;
use std::path::PathBuf;

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

  let compiled = to_core::book_to_hvm_core(&book)?;

  if args.compile {
    println!("{compiled}");
  }

  if args.run {
    todo!()
  }

  Ok(())
}
