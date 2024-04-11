use clap::{Args, CommandFactory, Parser, Subcommand};
use hvml::{
  check_book, compile_book, desugar_book,
  diagnostics::{Diagnostics, DiagnosticsConfig, Severity},
  load_file_to_book, run_book,
  term::{Book, Name},
  CompileOpts, OptLevel, RunOpts,
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
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,

    #[arg(value_parser = |arg: &str| hvml::term::parser::TermParser::new(arg).parse_term())]
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

    #[arg(short = 'p', help = "Debug and normalization pretty printing")]
    pretty: bool,

    #[arg(short = 'L', help = "Lazy mode")]
    lazy_mode: bool,

    #[command(flatten)]
    warn_opts: CliWarnOpts,

    #[arg(help = "Path to the input file")]
    path: PathBuf,
  },
}

#[derive(Args, Clone, Debug)]
struct RunArgs {
  #[arg(short = 'l', help = "Linear readback (show explicit dups)")]
  linear: bool,

  #[arg(short = 's', long = "stats", help = "Shows runtime stats and rewrite counts")]
  print_stats: bool,
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
  LinearizeMatches,
  LinearizeMatchesExtra,
  NoLinearizeMatches,
  FloatCombinators,
  NoFloatCombinators,
  Merge,
  NoMerge,
  Inline,
  NoInline,
}

fn compile_opts_from_cli(args: &Vec<OptArgs>, lazy_mode: bool) -> CompileOpts {
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
      FloatCombinators => opts.float_combinators = true,
      NoFloatCombinators => opts.float_combinators = false,
      Merge => opts.merge = true,
      NoMerge => opts.merge = false,
      Inline => opts.inline = true,
      NoInline => opts.inline = false,

      LinearizeMatches => opts.linearize_matches = OptLevel::Enabled,
      LinearizeMatchesExtra => opts.linearize_matches = OptLevel::Extra,
      NoLinearizeMatches => opts.linearize_matches = OptLevel::Disabled,
    }
  }

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
    Mode::Check { comp_opts, lazy_mode, warn_opts, path } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );
      let compile_opts = compile_opts_from_cli(&comp_opts, lazy_mode);

      let mut book = load_book(&path)?;
      let diagnostics = check_book(&mut book, diagnostics_cfg, compile_opts)?;
      eprintln!("{}", diagnostics);
    }

    Mode::Compile { path, comp_opts, warn_opts, lazy_mode } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );
      let opts = compile_opts_from_cli(&comp_opts, lazy_mode);

      let mut book = load_book(&path)?;
      let compile_res = compile_book(&mut book, opts, diagnostics_cfg, None)?;

      eprint!("{}", compile_res.diagnostics);
      println!("{}", compile_res.core_book);
    }

    Mode::Desugar { path, comp_opts, warn_opts, pretty, lazy_mode } => {
      let diagnostics_cfg = set_warning_cfg_from_cli(
        if lazy_mode { DiagnosticsConfig::default_lazy() } else { DiagnosticsConfig::default_strict() },
        lazy_mode,
        warn_opts,
      );

      let opts = compile_opts_from_cli(&comp_opts, lazy_mode);

      let mut book = load_book(&path)?;
      let diagnostics = desugar_book(&mut book, opts, diagnostics_cfg, None)?;

      eprint!("{diagnostics}");
      if pretty {
        println!("{}", book.display_pretty())
      } else {
        println!("{book}");
      }
    }

    Mode::Run { lazy_mode, run_opts, pretty, comp_opts, warn_opts, arguments, path } => {
      let RunArgs { linear, print_stats } = run_opts;

      let diagnostics_cfg =
        set_warning_cfg_from_cli(DiagnosticsConfig::new(Severity::Allow, arg_verbose), lazy_mode, warn_opts);

      let compile_opts = compile_opts_from_cli(&comp_opts, lazy_mode);

      compile_opts.check_for_strict();

      let run_opts = RunOpts { linear, lazy_mode, pretty };

      let book = load_book(&path)?;
      let (term, stats, diags) = run_book(book, run_opts, compile_opts, diagnostics_cfg, arguments)?;

      eprint!("{diags}");
      if pretty {
        println!("Result:\n{}", term.display_pretty(0));
      } else {
        println!("Result: {}", term);
      }
      if print_stats {
        println!("{stats}");
      }
    }
  };
  Ok(())
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
