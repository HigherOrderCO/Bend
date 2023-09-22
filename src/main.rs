#![feature(slice_group_by)]

use clap::{Parser, ValueEnum};
use hvm_core::show_lnet;
use hvm_lang::{check_book, compile_book, load_file_to_book, run_book, RunInfo};
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
      check_book(book)?;
    }
    Mode::Compile => {
      let (compiled, def_names) = compile_book(book)?;
      println!("{}", compiled.to_string(&def_names));
    }
    Mode::Run => {
      let (res_term, def_names, info) = run_book(book)?;
      let RunInfo { stats, valid_readback, lnet } = info;
      let rps = stats.rewrites as f64 / stats.run_time / 1_000_000.0;
      if args.verbose {
        println!("\n{}", show_lnet(&lnet));
      }

      if valid_readback {
        println!("\n{}\n", res_term.to_string(&def_names));
      } else {
        println!("\nInvalid readback from inet.");
        println!(" Got:\n{}\n", res_term.to_string(&def_names));
      }
      println!("size: {}", stats.size);
      println!("used: {}", stats.used);
      println!("Time: {:.3}s | Rwts: {} | RPS: {:.3}m", stats.run_time, stats.rewrites, rps);
    }
  }

  Ok(())
}
