use clap::{Args, CommandFactory, Parser, Subcommand};
use hvmc::ast::{show_book, show_net};
use hvml::{
  check_book, compile_book, desugar_book, load_file_to_book, run_book,
  term::{display::display_readback_errors, AdtEncoding, Name},
  CompileOpts, RunInfo, RunOpts, WarnState, WarningOpts,
};
use std::{path::PathBuf, vec::IntoIter};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  pub mode: Mode,

  #[arg(short, long, global = true)]
  pub verbose: bool,

  #[arg(short = 'e', long, global = true, help = "Use other entrypoint rather than main or Main")]
  pub entrypoint: Option<Name>,
}

#[derive(Subcommand, Clone, Debug)]
enum Mode {
  /// Checks that the program is syntactically and semantically correct.
  Check {
    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
  /// Compiles the program to hvmc and prints to stdout.
  Compile {
    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      supercombinators is enabled by default."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
  /// Compiles the program and runs it with the hvm.
  Run {
    #[arg(short, long, help = "How much memory to allocate for the runtime", default_value = "1G", value_parser = mem_parser)]
    mem: usize,

    #[arg(short = 'd', help = "Debug mode (print each reduction step)")]
    debug: bool,

    #[arg(short = '1', help = "Single-core mode (no parallelism)")]
    single_core: bool,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[arg(short = 'l', help = "Linear readback (show explicit dups)")]
    linear: bool,

    #[arg(short = 's', long = "stats", help = "Shows runtime stats and rewrite counts")]
    arg_stats: bool,

    #[arg(help = "Path to the input file")]
    path: PathBuf,

    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      supercombinators is enabled by default."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[command(flatten)]
    warn_opts: CliWarnOpts,
  },
  /// Runs the lambda-term level desugaring passes.
  Desugar {
    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      supercombinators is enabled by default."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
}

#[derive(Args, Debug, Clone)]
#[group(multiple = true)]
struct CliWarnOpts {
  #[arg(
    short = 'W',
    long = "warn",
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    help = "Show the specified compilation warning",
  )]
  pub warns: Vec<WarningArgs>,

  #[arg(
    short = 'D',
    long = "deny",
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    help = "Deny the specified compilation warning",
  )]
  pub denies: Vec<WarningArgs>,

  #[arg(
    short = 'A',
    long = "allow",
    value_delimiter = ' ',
    action = clap::ArgAction::Append,
    help = "Allow the specified compilation warning",
  )]
  pub allows: Vec<WarningArgs>,
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

    let cli = Cli::parse();
    let arg_verbose = cli.verbose;

    let verbose = |book: &_| {
      if arg_verbose {
        println!("{book:?}");
      }
    };

    execute_cli_mode(cli, &verbose)?;

    Ok(())
  }
  if let Err(e) = run() {
    eprintln!("{e}");
  }
}

fn execute_cli_mode(cli: Cli, verbose: &dyn Fn(&hvml::term::Book)) -> Result<(), String> {
  match cli.mode {
    Mode::Check { path } => {
      let book = load_file_to_book(&path)?;
      verbose(&book);
      check_book(book)?;
    }
    Mode::Compile { path, comp_opts, warn_opts, lazy_mode } => {
      let warning_opts = warn_opts.get_warning_opts(WarningOpts::default());
      let mut opts = OptArgs::opts_from_cli(&comp_opts);

      if lazy_mode {
        opts.lazy_mode()
      }

      let mut book = load_file_to_book(&path)?;
      verbose(&book);
      let compiled = compile_book(&mut book, opts, cli.entrypoint)?;
      hvml::display_warnings(&compiled.warnings, warning_opts)?;
      print!("{}", show_book(&compiled.core_book));
    }
    Mode::Desugar { path, comp_opts } => {
      let mut book = load_file_to_book(&path)?;
      verbose(&book);

      let opts = OptArgs::opts_from_cli(&comp_opts);

      desugar_book(&mut book, opts, None)?;
      println!("{book}");
    }
    Mode::Run { path, mem, debug, mut single_core, linear, arg_stats, comp_opts, warn_opts, lazy_mode } => {
      if debug && lazy_mode {
        return Err("Unsupported configuration, can not use debug mode `-d` with lazy mode `-L`".to_string());
      }

      let warning_opts = warn_opts.get_warning_opts(WarningOpts::allow_all());
      let mut opts = OptArgs::opts_from_cli(&comp_opts);
      opts.check(lazy_mode);

      if lazy_mode {
        single_core = true;
        opts.lazy_mode()
      }

      let book = load_file_to_book(&path)?;
      verbose(&book);

      let mem_size = mem / std::mem::size_of::<(hvmc::run::APtr, hvmc::run::APtr)>();
      let run_opts = RunOpts { single_core, debug, linear, lazy_mode };
      let (res_term, RunInfo { stats, readback_errors, net }) =
        run_book(book, mem_size, run_opts, warning_opts, opts, cli.entrypoint)?;

      let total_rewrites = stats.rewrites.total() as f64;
      let rps = total_rewrites / stats.run_time / 1_000_000.0;

      if cli.verbose {
        println!("\n{}", show_net(&net));
      }

      println!("{}{}", display_readback_errors(&readback_errors), res_term);

      if arg_stats {
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
  };
  Ok(())
}

impl CliWarnOpts {
  fn get_warning_opts(self, mut warning_opts: WarningOpts) -> WarningOpts {
    let cmd = Cli::command();
    let matches = cmd.get_matches();

    let subcmd_name = matches.subcommand_name().expect("To have a subcommand");
    let argm = matches.subcommand_matches(subcmd_name).expect("To have a subcommand");

    if let Some(wopts_id_seq) = argm.get_many::<clap::Id>("CliWarnOpts") {
      let allows = &mut self.allows.into_iter();
      let denies = &mut self.denies.into_iter();
      let warns = &mut self.warns.into_iter();
      WarningArgs::wopts_from_cli(&mut warning_opts, wopts_id_seq.collect(), allows, denies, warns);
    }
    warning_opts
  }
}

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum OptArgs {
  All,
  NoAll,
  Eta,
  NoEta,
  Prune,
  NoPrune,
  RefToRef,
  NoRefToRef,
  PreReduce,
  NoPreReduce,
  Supercombinators,
  NoSupercombinators,
  SimplifyMain,
  NoSimplifyMain,
  PreReduceRefs,
  NoPreReduceRefs,
  Merge,
  NoMerge,
  Inline,
  NoInline,
  AdtScott,
  AdtTaggedScott,
}

impl OptArgs {
  fn opts_from_cli(args: &Vec<Self>) -> CompileOpts {
    use OptArgs::*;
    let mut opts = CompileOpts::light();
    for arg in args {
      match arg {
        All => opts = CompileOpts::heavy(),
        NoAll => opts = CompileOpts::default(),
        Eta => opts.eta = true,
        NoEta => opts.eta = false,
        Prune => opts.prune = true,
        NoPrune => opts.prune = false,
        RefToRef => opts.ref_to_ref = true,
        NoRefToRef => opts.ref_to_ref = false,
        PreReduce => opts.pre_reduce = true,
        NoPreReduce => opts.pre_reduce = false,
        Supercombinators => opts.supercombinators = true,
        NoSupercombinators => opts.supercombinators = false,
        SimplifyMain => opts.simplify_main = true,
        NoSimplifyMain => opts.simplify_main = false,
        PreReduceRefs => opts.pre_reduce_refs = true,
        NoPreReduceRefs => opts.pre_reduce_refs = false,
        Merge => opts.merge = true,
        NoMerge => opts.merge = false,
        Inline => opts.inline = true,
        NoInline => opts.inline = false,
        AdtScott => opts.adt_encoding = AdtEncoding::Scott,
        AdtTaggedScott => opts.adt_encoding = AdtEncoding::TaggedScott,
      }
    }
    opts
  }
}

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum WarningArgs {
  All,
  UnusedDefs,
  MatchOnlyVars,
}

impl WarningArgs {
  pub fn wopts_from_cli(
    wopts: &mut WarningOpts,
    wopts_id_seq: Vec<&clap::Id>,
    allows: &mut IntoIter<WarningArgs>,
    denies: &mut IntoIter<WarningArgs>,
    warns: &mut IntoIter<WarningArgs>,
  ) {
    for id in wopts_id_seq {
      match id.as_ref() {
        "allows" => Self::set(wopts, allows.next().unwrap(), WarningOpts::allow_all, WarnState::Allow),
        "denies" => Self::set(wopts, denies.next().unwrap(), WarningOpts::deny_all, WarnState::Deny),
        "warns" => Self::set(wopts, warns.next().unwrap(), WarningOpts::warn_all, WarnState::Warn),
        _ => {}
      }
    }
  }

  fn set(wopts: &mut WarningOpts, val: WarningArgs, all: impl Fn() -> WarningOpts, switch: WarnState) {
    match val {
      WarningArgs::All => *wopts = all(),
      WarningArgs::UnusedDefs => wopts.unused_defs = switch,
      WarningArgs::MatchOnlyVars => wopts.match_only_vars = switch,
    }
  }
}
