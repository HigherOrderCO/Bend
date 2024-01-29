use clap::{Parser, ValueEnum};
use hvmc::ast::{show_book, show_net};
use hvml::{
  check_book, compile_book, desugar_book, load_file_to_book, run_book, total_rewrites, Opts, RunInfo,
  WarningOpts,
};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[arg(value_enum)]
  pub mode: Mode,

  #[arg(short, long)]
  pub verbose: bool,

  #[arg(
    short = 'D',
    long = "deny",
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    long_help = r#"Deny compilation warnings
    all, unused-defs, match-only-vars"#
  )]
  pub deny_warnings: Vec<String>,

  #[arg(
    short = 'A',
    long = "allow",
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    long_help = r#"Allow compilation warnings
    all, unused-defs, match-only-vars"#
  )]
  pub allow_warnings: Vec<String>,

  #[arg(long, help = "Stops if any warning was produced")]
  pub fatal_warnings: bool,

  #[arg(short, long, help = "Shows runtime stats and rewrite counts")]
  pub stats: bool,

  #[arg(short, long, help = "How much memory to allocate for the runtime", default_value = "1G", value_parser = mem_parser)]
  pub mem: usize,

  #[arg(short = '1', help = "Single-core mode (no parallelism)")]
  pub single_core: bool,

  #[arg(short = 'd', help = "Debug mode (print each reduction step)")]
  pub debug: bool,

  #[arg(short = 'l', help = "Linear readback (show explicit dups)")]
  pub linear: bool,

  #[arg(
    short = 'O',
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    long_help = r#"Enables or disables the given optimizations
    all, eta, no-eta, prune, no-prune, ref-to-ref, no-ref-to-ref,
    supercombinators (enabled by default), no-supercombinators,
    simplify-main, no-simplify-main, pre-reduce-refs, no-pre-reduce-refs
    "#,
  )]
  pub opts: Vec<String>,

  #[arg(help = "Path to the input file")]
  pub path: PathBuf,
}

#[derive(ValueEnum, Clone, Debug)]
enum Mode {
  /// Checks that the program is syntactically and semantically correct.
  Check,
  /// Compiles the program to hvmc and prints to stdout.
  Compile,
  /// Compiles the program and runs it with the hvm.
  Run,
  /// Runs the lambda-term level optimization passes.
  Desugar,
}

fn mem_parser(arg: &str) -> Result<usize, String> {
  let (base, mult) = match arg.to_lowercase().chars().last() {
    None => return Err("Mem size argument is empty".to_string()),
    Some('k') => (&arg[0 .. arg.len() - 1], 1 << 10),
    Some('m') => (&arg[0 .. arg.len() - 1], 1 << 20),
    Some('g') => (&arg[0 .. arg.len() - 1], 1 << 30),
    Some(_) => (arg, 1),
  };
  let base = base.parse::<usize>().map_err(|e| e.to_string())?;
  Ok(base * mult)
}

fn main() {
  fn run() -> Result<(), String> {
    #[cfg(not(feature = "cli"))]
    compile_error!("The 'cli' feature is needed for the hvm-lang cli");

    let args = Args::parse();
    let mut opts = Opts::light();
    Opts::from_vec(&mut opts, args.opts)?;
    let mut warning_opts = WarningOpts::default();
    WarningOpts::deny(&mut warning_opts, args.deny_warnings)?;
    WarningOpts::allow(&mut warning_opts, args.allow_warnings)?;

    let mut book = load_file_to_book(&args.path)?;
    if args.verbose {
      println!("{book:?}");
    }

    match args.mode {
      Mode::Check => {
        check_book(book)?;
      }
      Mode::Compile => {
        let compiled = compile_book(&mut book, opts)?;

        for warn in &compiled.warnings {
          eprintln!("WARNING: {warn}");
        }

        print!("{}", show_book(&compiled.core_book));
      }
      Mode::Desugar => {
        desugar_book(&mut book, opts)?;
        println!("{book}");
      }
      Mode::Run => {
        let mem_size = args.mem / std::mem::size_of::<(hvmc::run::APtr, hvmc::run::APtr)>();
        let (res_term, def_names, info) = run_book(
          book,
          mem_size,
          !args.single_core,
          args.debug,
          args.linear,
          args.fatal_warnings,
          warning_opts,
          opts,
        )?;
        let RunInfo { stats, readback_errors, net: lnet } = info;
        let total_rewrites = total_rewrites(&stats.rewrites) as f64;
        let rps = total_rewrites / stats.run_time / 1_000_000.0;
        if args.verbose {
          println!("\n{}", show_net(&lnet));
        }

        println!("{}{}", readback_errors.display(&def_names), res_term.display(&def_names));

        if args.stats {
          println!("\nRWTS   : {}", total_rewrites);
          println!("- ANNI : {}", stats.rewrites.anni);
          println!("- COMM : {}", stats.rewrites.comm);
          println!("- ERAS : {}", stats.rewrites.eras);
          println!("- DREF : {}", stats.rewrites.dref);
          println!("- OPER : {}", stats.rewrites.oper);
          println!("TIME   : {:.3} s", stats.run_time);
          println!("RPS    : {:.3} m", rps);
        }
      }
    }
    Ok(())
  }
  if let Err(e) = run() {
    eprintln!("{e}");
  }
}
