#![feature(box_patterns)]
#![feature(let_chains)]
#![allow(incomplete_features, clippy::missing_safety_doc, clippy::new_ret_no_self)]
#![feature(generic_const_exprs)]

use crate::fun::{book_to_nets, net_to_term::net_to_term, term_to_net::Labels, Book, Ctx, Term};
use diagnostics::{Diagnostics, DiagnosticsConfig, ERR_INDENT_SIZE};
use hvm::{
  add_recursive_priority::add_recursive_priority,
  ast::Net,
  check_net_size::{check_net_sizes, MAX_NET_SIZE},
  mutual_recursion,
};
use net::hvmc_to_net::hvmc_to_net;
use std::{process::Output, str::FromStr};

pub mod diagnostics;
pub mod fun;
pub mod hvm;
pub mod imp;
pub mod net;

pub use fun::load_book::load_file_to_book;

pub const ENTRY_POINT: &str = "main";
pub const HVM1_ENTRY_POINT: &str = "Main";

pub fn check_book(
  book: &mut Book,
  diagnostics_cfg: DiagnosticsConfig,
  compile_opts: CompileOpts,
) -> Result<Diagnostics, Diagnostics> {
  // TODO: Do the checks without having to do full compilation
  let res = compile_book(book, compile_opts, diagnostics_cfg, None)?;
  Ok(res.diagnostics)
}

pub fn compile_book(
  book: &mut Book,
  opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
) -> Result<CompileResult, Diagnostics> {
  let mut diagnostics = desugar_book(book, opts.clone(), diagnostics_cfg, args)?;

  let (mut hvm_book, labels) = book_to_nets(book, &mut diagnostics)?;

  if opts.eta {
    hvm_book.values_mut().for_each(hvm::ast::Net::eta_reduce);
  }

  mutual_recursion::check_cycles(&hvm_book, &mut diagnostics)?;
  if opts.eta {
    hvm_book.values_mut().for_each(hvm::ast::Net::eta_reduce);
  }

  if opts.inline {
    diagnostics.start_pass();
    if let Err(e) = hvm_book.inline() {
      diagnostics.add_book_error(format!("During inlining:\n{:ERR_INDENT_SIZE$}{}", "", e));
    }
    diagnostics.fatal(())?;
  }

  if opts.prune {
    let prune_entrypoints = vec![book.hvmc_entrypoint().to_string()];
    hvm_book.prune(&prune_entrypoints);
  }

  if opts.check_net_size {
    check_net_sizes(&hvm_book, &mut diagnostics)?;
  }

  add_recursive_priority(&mut hvm_book);

  Ok(CompileResult { core_book: hvm_book, labels, diagnostics })
}

pub fn desugar_book(
  book: &mut Book,
  opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
) -> Result<Diagnostics, Diagnostics> {
  let mut ctx = Ctx::new(book, diagnostics_cfg);

  ctx.check_shared_names();

  ctx.set_entrypoint();

  ctx.book.encode_adts(opts.adt_encoding);

  ctx.fix_match_defs()?;

  ctx.apply_args(args)?;

  ctx.desugar_open()?;

  ctx.book.encode_builtins();

  ctx.resolve_refs()?;

  ctx.desugar_match_defs()?;

  ctx.fix_match_terms()?;

  ctx.desugar_bend()?;
  ctx.desugar_fold()?;
  ctx.desugar_do_blocks()?;

  ctx.check_unbound_vars()?;

  ctx.book.make_var_names_unique();

  // Auto match linearization
  match opts.linearize_matches {
    OptLevel::Disabled => (),
    OptLevel::Alt => ctx.book.linearize_match_binds(),
    OptLevel::Enabled => ctx.book.linearize_matches(),
  }
  // Manual match linearization
  ctx.book.linearize_match_with();

  ctx.book.encode_matches(opts.adt_encoding);

  // sanity check
  ctx.check_unbound_vars()?;

  ctx.book.make_var_names_unique();
  ctx.book.desugar_use();
  ctx.book.make_var_names_unique();
  ctx.book.linearize_vars();

  // sanity check
  ctx.check_unbound_vars()?;

  // Optimizing passes
  if opts.float_combinators {
    ctx.book.float_combinators(MAX_NET_SIZE);
  }

  ctx.prune(opts.prune);

  if opts.merge {
    ctx.book.merge_definitions();
  }

  ctx.book.make_var_names_unique();

  if !ctx.info.has_errors() { Ok(ctx.info) } else { Err(ctx.info) }
}

pub fn run_book(
  mut book: Book,
  run_opts: RunOpts,
  compile_opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
  cmd: &str,
  arg_io: bool,
) -> Result<Option<(Term, String, Diagnostics)>, Diagnostics> {
  let CompileResult { core_book, labels, diagnostics } =
    compile_book(&mut book, compile_opts.clone(), diagnostics_cfg, args)?;

  // TODO: Printing should be taken care by the cli module, but we'd
  // like to print any warnings before running so that the user can
  // cancel the run if a problem is detected.
  eprint!("{diagnostics}");

  let out_path = ".out.hvm";
  std::fs::write(out_path, core_book.to_string()).map_err(|x| x.to_string())?;
  let run_fn = |out_path: &str| {
    let mut process = std::process::Command::new("hvm");
    process.arg(cmd).arg(out_path);
    if arg_io {
      process.arg("--io");
      process.stdout(std::process::Stdio::inherit());
      process.spawn()?.wait_with_output()
    } else {
      process.output()
    }
  };
  let Output { status, stdout, stderr } = run_fn(out_path).map_err(|e| format!("While running hvm: {e}"))?;

  let out = String::from_utf8_lossy(&stdout);
  let err = String::from_utf8_lossy(&stderr);
  let status = if !status.success() { status.to_string() } else { String::new() };

  let _ = std::fs::remove_file(out_path);

  if arg_io {
    return Ok(None);
  }

  let Some((_, result)) = out.split_once("Result: ") else {
    return Err(
      format!("1.Failed to parse result from HVM.\nOutput from HVM was:\n{:?}{:?}{:?}", err, status, out)
        .into(),
    );
  };
  let Some((result, stats)) = result.split_once('\n') else {
    return Err(
      format!("2.Failed to parse result from HVM.\nOutput from HVM was:\n{:?}{:?}{:?}", err, status, out)
        .into(),
    );
  };
  let net = match hvm::ast::Net::from_str(result) {
    Ok(net) => net,
    Err(e) => {
      return Err(
        format!(
          "3.Failed to parse result from HVM with error: '{}'.\nOutput from HVM was:\n{:?}{:?}{:?}",
          e, err, status, out
        )
        .into(),
      );
    }
  };

  let (term, diags) =
    readback_hvm_net(&net, &book, &labels, run_opts.linear_readback, compile_opts.adt_encoding);
  Ok(Some((term, stats.to_string(), diags)))
}

pub fn readback_hvm_net(
  net: &Net,
  book: &Book,
  labels: &Labels,
  linear: bool,
  adt_encoding: AdtEncoding,
) -> (Term, Diagnostics) {
  let mut diags = Diagnostics::default();
  let net = hvmc_to_net(net);
  let mut term = net_to_term(&net, book, labels, linear, &mut diags);
  term.expand_generated(book);
  term.resugar_strings(adt_encoding);
  term.resugar_lists(adt_encoding);
  (term, diags)
}

#[derive(Clone, Copy, Debug, Default)]
pub struct RunOpts {
  pub linear_readback: bool,
  pub pretty: bool,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum OptLevel {
  Disabled,
  #[default]
  Enabled,
  Alt,
}

impl OptLevel {
  pub fn enabled(&self) -> bool {
    !matches!(self, OptLevel::Disabled)
  }

  pub fn is_extra(&self) -> bool {
    matches!(self, OptLevel::Enabled)
  }
}

#[derive(Clone, Debug)]
pub struct CompileOpts {
  /// Enables [hvmc::transform::eta_reduce].
  pub eta: bool,

  /// Enables [fun::transform::definition_pruning] and [hvmc_net::prune].
  pub prune: bool,

  /// Enables [fun::transform::linearize_matches].
  pub linearize_matches: OptLevel,

  /// Enables [fun::transform::float_combinators].
  pub float_combinators: bool,

  /// Enables [fun::transform::definition_merge]
  pub merge: bool,

  /// Enables [hvmc::transform::inline].
  pub inline: bool,

  /// Enables [hvm::check_net_size].
  pub check_net_size: bool,

  /// Determines the encoding of constructors and matches.
  pub adt_encoding: AdtEncoding,
}

impl CompileOpts {
  /// Set all optimizing options as true
  #[must_use]
  pub fn set_all(self) -> Self {
    Self {
      eta: true,
      prune: true,
      float_combinators: true,
      merge: true,
      inline: true,
      linearize_matches: OptLevel::Enabled,
      check_net_size: self.check_net_size,
      adt_encoding: self.adt_encoding,
    }
  }

  /// Set all optimizing options as false
  #[must_use]
  pub fn set_no_all(self) -> Self {
    Self {
      eta: false,
      prune: false,
      linearize_matches: OptLevel::Disabled,
      float_combinators: false,
      merge: false,
      inline: false,
      check_net_size: self.check_net_size,
      adt_encoding: self.adt_encoding,
    }
  }

  pub fn check_for_strict(&self) {
    if !self.float_combinators {
      println!(
        "Warning: Running in strict mode without enabling the float_combinators pass can lead to some functions expanding infinitely."
      );
    }
    if !self.linearize_matches.enabled() {
      println!(
        "Warning: Running in strict mode without enabling the linearize_matches pass can lead to some functions expanding infinitely."
      );
    }
  }
}

impl Default for CompileOpts {
  /// Enables eta, linearize_matches, float_combinators.
  /// Uses num-scott ADT encoding.
  fn default() -> Self {
    Self {
      eta: true,
      prune: false,
      linearize_matches: OptLevel::Enabled,
      float_combinators: true,
      merge: false,
      inline: false,
      check_net_size: false,
      adt_encoding: AdtEncoding::NumScott,
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub enum AdtEncoding {
  Scott,
  NumScott,
}

impl std::fmt::Display for AdtEncoding {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AdtEncoding::Scott => write!(f, "Scott"),
      AdtEncoding::NumScott => write!(f, "NumScott"),
    }
  }
}

pub struct CompileResult {
  pub diagnostics: Diagnostics,
  pub core_book: hvm::ast::Book,
  pub labels: Labels,
}

fn maybe_grow<R, F>(f: F) -> R
where
  F: FnOnce() -> R,
{
  stacker::maybe_grow(1024 * 32, 1024 * 1024, f)
}
