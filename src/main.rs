#![feature(slice_group_by)]

use clap::{Parser, ValueEnum};
use hvm_lang::{check_book, compile_book, load_file_to_book, run_book, RunInfo};
use hvmc::{show_lbook, show_lnet};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[arg(value_enum)]
  pub mode: Mode,

  #[arg(short, long)]
  pub verbose: bool,

  #[arg(long, help = "How much memory to allocate for the runtime", default_value = "1G", value_parser = mem_parser)]
  pub mem: usize,

  #[arg(help = "Path to the input file")]
  pub path: PathBuf,
}

#[derive(ValueEnum, Clone, Debug)]
enum Mode {
  Check,
  Compile,
  Run,
}

fn mem_parser(arg: &str) -> anyhow::Result<usize> {
  let bytes = byte_unit::Byte::from_str(arg)?;
  Ok(bytes.get_bytes() as usize)
}

fn main() -> anyhow::Result<()> {
  #[cfg(not(feature = "cli"))]
  compile_error!("The 'cli' feature is needed for the hvm-lang cli");

  let args = Args::parse();

  let mut book = load_file_to_book(&args.path)?;
  if args.verbose {
    println!("{book:?}");
  }

  match args.mode {
    Mode::Check => {
      check_book(book)?;
    }
    Mode::Compile => {
      let compiled = compile_book(&mut book)?;
      println!("{}", show_lbook(&compiled));
    }
    Mode::Run => {
      let (res_term, def_names, info) = run_book(book, args.mem / std::mem::size_of::<u64>())?;
      let RunInfo { stats, valid_readback, lnet } = info;
      let rps = stats.rewrites.total_rewrites() as f64 / stats.run_time / 1_000_000.0;
      if args.verbose {
        println!("\n{}", show_lnet(&lnet));
      }

      if valid_readback {
        println!("\n{}\n", res_term.to_string(&def_names));
      } else {
        println!("\nInvalid readback from inet.");
        println!(" Got:\n{}\n", res_term.to_string(&def_names));
      }
      // TODO: Some way to figure out total memory use?
      // println!("size: {}", stats.size);
      println!("used: {}", stats.used);
      println!(
        "Time: {:.3}s | Anni: {:.3} | Comm: {:.3} | Eras: {:.3} | Dref: {:.3} | RPS: {:.3}m",
        stats.run_time,
        stats.rewrites.anni,
        stats.rewrites.comm,
        stats.rewrites.eras,
        stats.rewrites.dref,
        rps
      );
    }
  }

  Ok(())
}
