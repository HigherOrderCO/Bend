#![feature(slice_group_by)]

use clap::{Parser, ValueEnum};
use hvm_lang::{check_book, compile_book, load_file_to_book, run_book};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[arg(value_enum)]
  pub mode: Mode,

  #[arg(short, long)]
  pub verbose: bool,

  #[arg(help = "Path to the input file")]
  pub path: PathBuf,
}

#[derive(ValueEnum, Clone, Debug)]
enum Mode {
  Check,
  Compile,
  Run,
}

fn main() -> anyhow::Result<()> {
  let args = Args::parse();

  let book = load_file_to_book(&args.path)?;
  if args.verbose {
    println!("{book:?}");
  }

  match args.mode {
    Mode::Check => {
      check_book(&book)?;
    }
    Mode::Compile => {
      let compiled = compile_book(&book)?;
      println!("{compiled}");
    }
    Mode::Run => {
      let (res, stats) = run_book(&book)?;
      let rps = stats.rewrites as f64 / stats.run_time / 1_000_000.0;
      println!("\n{}\n", res);
      println!("size: {}", stats.size);
      println!("used: {}", stats.used);
      println!("Time: {:.3}s | Rwts: {} | RPS: {:.3}m", stats.run_time, stats.rewrites, rps);
    }
  }

  Ok(())
}
