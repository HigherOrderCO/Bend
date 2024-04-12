use clap::{Args, CommandFactory, Parser, Subcommand};
use hvml::{
  check_book, compile_book, desugar_book,
  diagnostics::{Diagnostics, DiagnosticsConfig, Severity},
  hvmc_net::pre_reduce::MAX_REWRITES_DEFAULT,
  load_file_to_book, run_book,
  term::{AdtEncoding, Book, Name},
  CompileOpts, OptLevel, RunInfo, RunOpts,
};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
  #[command(subcommand)]
  pub mode: Mode,

  #[arg(short, long, global = true)]
  pub verbose: bool,

  #[arg(short = 'e', long, global = true, help = "Use other entrypoint rather than main or Main")]
  pub entrypoint: Option<String>,
}

#[derive(Subcommand, Clone, Debug)]
enum Mode {
  /// Checks that the program is syntactically and semantically correct.
  Check {
    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      float_combinators is enabled by default on strict mode."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[command(flatten)]
    transform_opts: TransformOpts,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

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
      float_combinators is enabled by default on strict mode."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[command(flatten)]
    transform_opts: TransformOpts,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
  /// Compiles the program and runs it with the hvm.
  Run {
    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[arg(short = 'p', help = "Debug and normalization pretty printing")]
    pretty: bool,

    #[command(flatten)]
    run_opts: RunArgs,

    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      float_combinators is enabled by default on strict mode."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[command(flatten)]
    transform_opts: TransformOpts,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,

    #[arg(value_parser = |arg: &str| hvml::term::parser::TermParser::new_term(arg))]
    arguments: Option<Vec<hvml::term::Term>>,
  },
  /// Runs the lambda-term level desugaring passes.
  Desugar {
    #[arg(
      short = 'O',
      value_delimiter = ' ',
      action = clap::ArgAction::Append,
      long_help = r#"Enables or disables the given optimizations
      float_combinators is enabled by default on strict mode."#,
    )]
    comp_opts: Vec<OptArgs>,

    #[command(flatten)]
    transform_opts: TransformOpts,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
}

#[derive(Args, Clone, Debug)]
struct TransformOpts {
  /// Names of the definitions that should not get pre-reduced.
  ///
  /// For programs that don't take arguments
  /// and don't have side effects this is usually the entry point of the
  /// program (otherwise, the whole program will get reduced to normal form).
  #[arg(long = "pre-reduce-skip", value_delimiter = ' ', action = clap::ArgAction::Append)]
  pre_reduce_skip: Vec<String>,

  /// How much memory to allocate when pre-reducing.
  /// If not specified, allocate an amount proportional to the rewrite limit.
  ///
  /// Supports abbreviations such as '4G' or '400M'.
  #[arg(long = "pre-reduce-memory", value_parser = parse_abbrev_number::<usize>)]
  pre_reduce_memory: Option<usize>,

  /// Maximum amount of rewrites to do when pre-reducing.
  ///
  /// Supports abbreviations such as '4G' or '400M'.
  #[arg(long = "pre-reduce-rewrites", default_value_t = MAX_REWRITES_DEFAULT, value_parser = parse_abbrev_number::<u64>)]
  pre_reduce_rewrites: u64,
}
#[derive(Args, Clone, Debug)]
struct RunArgs {
  #[arg(short = 'm', long = "mem", help = "How much memory to allocate for the runtime", value_parser = parse_abbrev_number::<usize>)]
  max_memory: Option<usize>,

  #[arg(short = 'r', long = "rwts", help = "Maximum amount of rewrites", value_parser = parse_abbrev_number::<usize>)]
  max_rewrites: Option<usize>,

  #[arg(short = 'd', help = "Debug mode (print each reduction step)")]
  debug: bool,

  #[arg(short = '1', help = "Single-core mode (no parallelism)")]
  single_core: bool,

  #[arg(short = 'l', help = "Linear readback (show explicit dups)")]
  linear: bool,

  #[arg(short = 's', long = "stats", help = "Shows runtime stats and rewrite counts")]
  arg_stats: bool,
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

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum OptArgs {
  All,
  NoAll,
  Eta,
  NoEta,
  Prune,
  NoPrune,
  PreReduce,
  NoPreReduce,
  LinearizeMatches,
  LinearizeMatchesExtra,
  NoLinearizeMatches,
  FloatCombinators,
  NoFloatCombinators,
  Merge,
  NoMerge,
  Inline,
  NoInline,
  AdtScott,
  AdtTaggedScott,
}

fn compile_opts_from_cli(args: &Vec<OptArgs>, transform_opts: TransformOpts, lazy_mode: bool) -> CompileOpts {
  use OptArgs::*;
  let mut opts = if lazy_mode { CompileOpts::default_lazy() } else { CompileOpts::default_strict() };

  for arg in args {
    match arg {
      All => opts = opts.set_all(),
      NoAll => opts = opts.set_no_all(),
      Eta => opts.eta = true,
      NoEta => opts.eta = false,
      Prune => opts.prune = true,
      NoPrune => opts.prune = false,
      PreReduce => opts.pre_reduce = true,
      NoPreReduce => opts.pre_reduce = false,
      FloatCombinators => opts.float_combinators = true,
      NoFloatCombinators => opts.float_combinators = false,
      Merge => opts.merge = true,
      NoMerge => opts.merge = false,
      Inline => opts.inline = true,
      NoInline => opts.inline = false,

      AdtScott => opts.adt_encoding = AdtEncoding::Scott,
      AdtTaggedScott => opts.adt_encoding = AdtEncoding::TaggedScott,

      LinearizeMatches => opts.linearize_matches = OptLevel::Enabled,
      LinearizeMatchesExtra => opts.linearize_matches = OptLevel::Extra,
      NoLinearizeMatches => opts.linearize_matches = OptLevel::Disabled,
    }
  }

  opts.pre_reduce_memory = transform_opts.pre_reduce_memory;
  opts.pre_reduce_rewrites = transform_opts.pre_reduce_rewrites;
  opts.pre_reduce_skip = transform_opts.pre_reduce_skip.into_iter().map(Name::new).collect();

  opts
}

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum WarningArgs {
  All,
  IrrefutableMatch,
  RedundantMatch,
  UnreachableMatch,
  UnusedDefinition,
  RepeatedBind,
  RecursionCycle,
  RecursionPreReduce,
}

fn main() {
  #[cfg(not(feature = "cli"))]
  compile_error!("The 'cli' feature is needed for the hvm-lang cli");

  let cli = Cli::parse();

  if let Err(diagnostics) = execute_cli_mode(cli) {
    eprint!("{diagnostics}")
  }
}

fn execute_cli_mode(mut cli: Cli) -> Result<(), Diagnostics> {
  let arg_verbose = cli.verbose;
  let entrypoint = cli.entrypoint.take();

  let load_book = |path: &Path| -> Result<Book, Diagnostics> {
    let mut book = load_file_to_book(path)?;
    book.entrypoint = entrypoint.map(Name::new);

    if arg_verbose {
      println!("{book}");
    }

    Ok(book)
  };

  match cli.mode {
    Mode::Check { comp_opts, transform_opts, lazy_mode, warn_opts, path } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );
      let compile_opts = compile_opts_from_cli(&comp_opts, transform_opts, lazy_mode);

      let mut book = load_book(&path)?;
      check_book(&mut book, diagnostics_cfg, compile_opts)?;
    }

    Mode::Compile { path, comp_opts, warn_opts, lazy_mode, transform_opts } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );
      let opts = compile_opts_from_cli(&comp_opts, transform_opts, lazy_mode);

      let mut book = load_book(&path)?;
      let compile_res = compile_book(&mut book, opts, diagnostics_cfg, None)?;

      eprint!("{}", compile_res.diagnostics);
      println!("{}", compile_res.core_book);
    }

    Mode::Desugar { path, comp_opts, warn_opts, lazy_mode, transform_opts } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );

      let opts = compile_opts_from_cli(&comp_opts, transform_opts, lazy_mode);

      let mut book = load_book(&path)?;
      let diagnostics = desugar_book(&mut book, opts, diagnostics_cfg, None)?;

      eprint!("{diagnostics}");
      println!("{book}");
    }

    Mode::Run { lazy_mode, run_opts, pretty, comp_opts, transform_opts, warn_opts, arguments, path } => {
      let RunArgs { max_memory, max_rewrites, debug, mut single_core, linear, arg_stats } = run_opts;

      let diagnostics_cfg =
        set_warning_cfg_from_cli(DiagnosticsConfig::new(Severity::Allow, arg_verbose), lazy_mode, warn_opts);

      let compile_opts = compile_opts_from_cli(&comp_opts, transform_opts, lazy_mode);

      if lazy_mode {
        if !single_core {
          eprintln!(
            "Warning: Parallel mode not yet implemented for lazy mode. Using single-core mode. Use the '-1' option to hide this message."
          );
        }
        single_core = true;
      } else {
        compile_opts.check_for_strict();
      }

      let run_opts = RunOpts { single_core, debug, linear, lazy_mode, max_memory, max_rewrites, pretty };

      let book = load_book(&path)?;
      let (res_term, RunInfo { stats, diagnostics, net, book: _, labels: _ }) =
        run_book(book, max_memory, run_opts, compile_opts, diagnostics_cfg, arguments)?;

      let total_rewrites = stats.rewrites.total() as f64;
      let rps = total_rewrites / stats.run_time / 1_000_000.0;
      let size = stats.used;

      if cli.verbose {
        println!("{net}");
      }

      eprint!("{diagnostics}");
      if pretty {
        println!("{}", res_term.pretty(0))
      } else {
        println!("{}", res_term);
      }


      if arg_stats {
        println!("\nRWTS   : {}", total_rewrites);
        println!("- ANNI : {}", stats.rewrites.anni);
        println!("- COMM : {}", stats.rewrites.comm);
        println!("- ERAS : {}", stats.rewrites.eras);
        println!("- DREF : {}", stats.rewrites.dref);
        println!("- OPER : {}", stats.rewrites.oper);
        println!("TIME   : {:.3} s", stats.run_time);
        println!("RPS    : {:.3} m", rps);
        println!("SIZE   : {} nodes", size);
      }
    }
  };
  Ok(())
}

/// Turn a string representation of a number, such as '1G' or '400K', into a
/// number.
///
/// This return a [`u64`] instead of [`usize`] to ensure that parsing CLI args
/// doesn't fail on 32-bit systems. We want it to fail later on, when attempting
/// to run the program.
fn parse_abbrev_number<T: TryFrom<u64>>(arg: &str) -> Result<T, String>
where
  <T as TryFrom<u64>>::Error: core::fmt::Debug,
{
  let (base, scale) = match arg.to_lowercase().chars().last() {
    None => return Err("Mem size argument is empty".to_string()),
    Some('k') => (&arg[0 .. arg.len() - 1], 1u64 << 10),
    Some('m') => (&arg[0 .. arg.len() - 1], 1u64 << 20),
    Some('g') => (&arg[0 .. arg.len() - 1], 1u64 << 30),
    Some('t') => (&arg[0 .. arg.len() - 1], 1u64 << 40),
    Some(_) => (arg, 1),
  };
  let base = base.parse::<u64>().map_err(|e| e.to_string())?;
  (base * scale).try_into().map_err(|e| format!("{:?}", e))
}

fn set_warning_cfg_from_cli(
  mut cfg: DiagnosticsConfig,
  lazy_mode: bool,
  warn_opts: CliWarnOpts,
) -> DiagnosticsConfig {
  fn set(cfg: &mut DiagnosticsConfig, severity: Severity, cli_val: WarningArgs, lazy_mode: bool) {
    match cli_val {
      WarningArgs::All => {
        cfg.irrefutable_match = severity;
        cfg.redundant_match = severity;
        cfg.unreachable_match = severity;
        cfg.unused_definition = severity;
        cfg.repeated_bind = severity;
        if !lazy_mode {
          cfg.recursion_cycle = severity;
        }
      }
      WarningArgs::IrrefutableMatch => cfg.irrefutable_match = severity,
      WarningArgs::RedundantMatch => cfg.redundant_match = severity,
      WarningArgs::UnreachableMatch => cfg.unreachable_match = severity,
      WarningArgs::UnusedDefinition => cfg.unused_definition = severity,
      WarningArgs::RepeatedBind => cfg.repeated_bind = severity,
      WarningArgs::RecursionCycle => cfg.recursion_cycle = severity,
      WarningArgs::RecursionPreReduce => cfg.recursion_pre_reduce = severity,
    }
  }

  let cmd = Cli::command();
  let matches = cmd.get_matches();
  let subcmd_name = matches.subcommand_name().expect("To have a subcommand");
  let arg_matches = matches.subcommand_matches(subcmd_name).expect("To have a subcommand");

  if let Some(warn_opts_ids) = arg_matches.get_many::<clap::Id>("CliWarnOpts") {
    let mut allows = warn_opts.allows.into_iter();
    let mut warns = warn_opts.warns.into_iter();
    let mut denies = warn_opts.denies.into_iter();
    for id in warn_opts_ids {
      match id.as_ref() {
        "allows" => set(&mut cfg, Severity::Allow, allows.next().unwrap(), lazy_mode),
        "denies" => set(&mut cfg, Severity::Error, denies.next().unwrap(), lazy_mode),
        "warns" => set(&mut cfg, Severity::Warning, warns.next().unwrap(), lazy_mode),
        _ => unreachable!(),
      }
    }
  }
  cfg
}
