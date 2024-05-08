#![feature(box_patterns)]
#![feature(let_chains)]

use crate::fun::{book_to_nets, net_to_term::net_to_term, term_to_net::Labels, Book, Ctx, Term};
use diagnostics::{Diagnostics, DiagnosticsConfig, ERR_INDENT_SIZE};
use hvmc::ast::Net;
use hvmc_net::{
  check_net_size::{check_net_sizes, MAX_NET_SIZE},
  mutual_recursion,
  reorder_redexes::reorder_redexes_recursive_last,
};
use net::hvmc_to_net::hvmc_to_net;
use std::{process::Output, str::FromStr};

pub mod diagnostics;
pub mod fun;
pub mod hvmc_net;
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

  let (mut core_book, labels) = book_to_nets(book, &mut diagnostics)?;

  if opts.eta {
    core_book.values_mut().for_each(Net::eta_reduce);
  }

  mutual_recursion::check_cycles(&core_book, &mut diagnostics)?;
  if opts.eta {
    core_book.values_mut().for_each(Net::eta_reduce);
  }

  if opts.inline {
    diagnostics.start_pass();
    if let Err(e) = core_book.inline() {
      diagnostics.add_book_error(format!("During inlining:\n{:ERR_INDENT_SIZE$}{}", "", e));
    }
    diagnostics.fatal(())?;
  }

  if opts.prune {
    let prune_entrypoints = vec![book.hvmc_entrypoint().to_string()];
    core_book.prune(&prune_entrypoints);
  }

  check_net_sizes(&core_book, &mut diagnostics)?;

  if opts.recursive_last {
    reorder_redexes_recursive_last(&mut core_book);
  }

  Ok(CompileResult { core_book, labels, diagnostics })
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

  ctx.book.encode_adts();

  ctx.fix_match_defs()?;

  ctx.apply_args(args)?;

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

  ctx.book.encode_matches();

  // sanity check
  ctx.check_unbound_vars()?;

  ctx.book.make_var_names_unique();
  ctx.book.apply_use();
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
) -> Result<(Term, String, Diagnostics), Diagnostics> {
  let CompileResult { core_book, labels, diagnostics } =
    compile_book(&mut book, compile_opts.clone(), diagnostics_cfg, args)?;

  // TODO: Printing should be taken care by the cli module, but we'd
  // like to print any warnings before running so that the user can
  // cancel the run if a problem is detected.
  eprint!("{diagnostics}");

  let out_path = ".out.hvm";
  std::fs::write(out_path, core_book.to_string()).map_err(|x| x.to_string())?;
  let Output { status, stdout, stderr } = std::process::Command::new("hvm")
    .arg("run")
    .arg(out_path)
    .output()
    .map_err(|x| format!("While running hvm: {x}"))?;

  let out = String::from_utf8_lossy(&stdout);
  let err = String::from_utf8_lossy(&stderr);
  let status = if !status.success() { status.to_string() } else { String::new() };

  let Some((_, result)) = out.split_once("Result: ") else {
    return Err(format!("Error reading result from hvm. Output :\n{}{}{}", err, status, out).into());
  };
  let Some((result, stats)) = result.split_once('\n') else {
    return Err(format!("Error reading result from hvm. Output :\n{}{}{}", err, status, out).into());
  };
  let Ok(net) = hvmc::ast::Net::from_str(result) else {
    return Err(format!("Error reading result from hvm. Output :\n{}{}{}", err, status, out).into());
  };

  let (term, diags) = readback_hvm_net(&net, &book, &labels, run_opts.linear_readback);
  Ok((term, stats.to_string(), diags))
}

pub fn readback_hvm_net(net: &Net, book: &Book, labels: &Labels, linear: bool) -> (Term, Diagnostics) {
  let mut diags = Diagnostics::default();
  let net = hvmc_to_net(net);
  let mut term = net_to_term(&net, book, labels, linear, &mut diags);
  term.expand_generated(book);
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
  /// Enables [fun::transform::eta_reduction].
  pub eta: bool,

  /// Enables [fun::transform::definition_pruning] and [hvmc_net::prune].
  pub prune: bool,

  /// Enables [fun::transform::linearize_matches].
  pub linearize_matches: OptLevel,

  /// Enables [fun::transform::float_combinators].
  pub float_combinators: bool,

  /// Enables [fun::transform::definition_merge]
  pub merge: bool,

  /// Enables [fun::transform::inline].
  pub inline: bool,

  /// Enables [hvmc_net::reorder_redexes::reorder_redexes_recursive_last].
  pub recursive_last: bool,
}

impl CompileOpts {
  /// Set all opts as true and keep the current adt encoding.
  #[must_use]
  pub fn set_all(self) -> Self {
    Self {
      eta: true,
      prune: true,
      float_combinators: true,
      merge: true,
      inline: true,
      recursive_last: true,
      linearize_matches: OptLevel::Enabled,
    }
  }

  /// Set all opts as false and keep the current adt encoding.
  #[must_use]
  pub fn set_no_all(self) -> Self {
    Self {
      eta: false,
      prune: false,
      linearize_matches: OptLevel::Disabled,
      float_combinators: false,
      merge: false,
      inline: false,
      recursive_last: false,
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
  /// Enables eta, linearize_matches, float_combinators and reorder_redexes_recursive_last.
  fn default() -> Self {
    Self {
      eta: true,
      prune: false,
      linearize_matches: OptLevel::Enabled,
      float_combinators: true,
      merge: false,
      inline: false,
      recursive_last: true,
    }
  }
}

pub struct CompileResult {
  pub diagnostics: Diagnostics,
  pub core_book: hvmc::ast::Book,
  pub labels: Labels,
}

fn maybe_grow<R, F>(f: F) -> R
where
  F: FnOnce() -> R,
{
  stacker::maybe_grow(1024 * 32, 1024 * 1024, f)
}
