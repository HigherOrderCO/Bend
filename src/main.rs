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
  path: PathBuf,
}

fn main() -> anyhow::Result<()> {
  let args = Args::parse();
  let book = loader::load_file_to_book(&args.path)?;
  println!("{book:?}");
  let compiled = to_core::book_to_hvm_core(&book)?;
  println!("{compiled}");
  Ok(())
}
