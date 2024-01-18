#![feature(box_patterns)]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

use hvmc::{
  ast::{book_to_runtime, show_book, Net},
  run::{Def, Rewrites},
};
use hvmc_net::{pre_reduce::pre_reduce_book, prune::prune_defs};
use itertools::Itertools;
use net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::nets_to_hvmc};
use std::time::Instant;
use term::{
  book_to_nets, net_to_term,
  term_to_net::{HvmcNames, Labels},
  Book, DefId, DefNames, Name, Term,
};

pub mod hvmc_net;
pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

use crate::term::net_to_term::ReadbackErrors;

pub fn check_book(mut book: Book) -> Result<(), String> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book, Opts::light())?;
  Ok(())
}

pub fn compile_book(book: &mut Book, opts: Opts) -> Result<CompileResult, String> {
  let (main, warnings) = desugar_book(book, opts)?;
  let (nets, hvmc_names, labels) = book_to_nets(book, main);
  let mut core_book = nets_to_hvmc(nets, &hvmc_names)?;
  if opts.pre_reduce && !opts.lazy {
    pre_reduce_book(&mut core_book, opts.pre_reduce)?;
  }
  if opts.prune {
    prune_defs(&mut core_book);
  }
  Ok(CompileResult { core_book, hvmc_names, labels, warnings })
}

pub fn desugar_book(book: &mut Book, opts: Opts) -> Result<(DefId, Vec<Warning>), String> {
  let mut warnings = Vec::new();
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.generate_scott_adts();
  book.encode_builtins();
  encode_pattern_matching(book, &mut warnings)?;
  // sanity check
  book.check_unbound_vars()?;
  book.normalize_native_matches()?;
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars();
  book.eta_reduction(opts.eta);
  // sanity check
  book.check_unbound_vars()?;
  if opts.supercombinators {
    book.detach_supercombinators(main);
  }
  if opts.ref_to_ref {
    book.simplify_ref_to_ref()?;
  }
  if opts.simplify_main {
    book.simplify_main_ref(main);
  }
  book.prune(Some(main), opts.prune, &mut warnings);
  if opts.merge_definitions {
    book.merge_definitions(main);
  }
  Ok((main, warnings))
}

pub fn encode_pattern_matching(book: &mut Book, warnings: &mut Vec<Warning>) -> Result<(), String> {
  book.resolve_ctrs_in_pats();
  book.check_unbound_pats()?;
  book.resolve_refs()?;
  book.desugar_let_destructors();
  book.desugar_implicit_match_binds();
  // This call to unbound vars needs to be after desugar_implicit_match_binds,
  // since we need the generated pattern names, like `x-1`, `ctr.field`.
  book.check_unbound_vars()?;
  book.extract_adt_matches(warnings)?;
  book.flatten_rules();
  let def_types = book.infer_def_types()?;
  book.check_exhaustive_patterns(&def_types)?;
  book.encode_pattern_matching_functions(&def_types);
  Ok(())
}

pub fn run_book(
  mut book: Book,
  mem_size: usize,
  parallel: bool,
  debug: bool,
  linear: bool,
  warning_opts: WarningOpts,
  opts: Opts,
) -> Result<(Term, DefNames, RunInfo), String> {
  let CompileResult { core_book, hvmc_names, labels, warnings } = compile_book(&mut book, opts)?;

  display_warnings(warning_opts, &warnings)?;

  fn debug_hook(net: &Net, book: &Book, hvmc_names: &HvmcNames, labels: &Labels, linear: bool) {
    let net = hvmc_to_net(net, &|id| hvmc_names.hvmc_name_to_id[&id]);
    let (res_term, errors) = net_to_term(&net, book, labels, linear);
    println!(
      "{}{}\n---------------------------------------",
      errors.display(&book.def_names),
      res_term.display(&book.def_names)
    );
  }
  let debug_hook =
    if debug { Some(|net: &_| debug_hook(net, &book, &hvmc_names, &labels, linear)) } else { None };

  let (res_lnet, stats) = run_compiled(&core_book, mem_size, parallel, opts.lazy, debug_hook);
  let net = hvmc_to_net(&res_lnet, &|id| hvmc_names.hvmc_name_to_id[&id]);
  let (res_term, readback_errors) = net_to_term(&net, &book, &labels, linear);
  let info = RunInfo { stats, readback_errors, net: res_lnet };
  Ok((res_term, book.def_names, info))
}

pub fn run_compiled(
  book: &hvmc::ast::Book,
  mem_size: usize,
  parallel: bool,
  lazy: bool,
  hook: Option<impl FnMut(&Net)>,
) -> (Net, RunStats) {
  let runtime_book = book_to_runtime(book);
  let root = &mut hvmc::run::Net::new(mem_size, lazy);

  let start_time = Instant::now();

  if let Some(mut hook) = hook {
    expand(root, &runtime_book);
    while !rdex(root).is_empty() {
      hook(&net_from_runtime(&root));
      reduce(root, &runtime_book, 1);
      expand(root, &runtime_book);
    }
  } else if parallel {
    root.parallel_normal(&runtime_book);
  } else {
    root.normal(&runtime_book);
  }

  let elapsed = start_time.elapsed().as_secs_f64();

  let net = net_from_runtime(&root);
  let def = runtime_net_to_runtime_def(&root);
  let stats = RunStats { rewrites: root.get_rewrites(), used: def.node.len(), run_time: elapsed };
  (net, stats)
}

pub fn total_rewrites(rwrts: &Rewrites) -> usize {
  rwrts.anni + rwrts.comm + rwrts.eras + rwrts.dref + rwrts.oper
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Opts {
  /// Enables [term::transform::eta_reduction].
  pub eta: bool,

  /// Enables [term::transform::simplify_ref_to_ref].
  pub ref_to_ref: bool,

  /// Enables [term::transform::definition_pruning] and [hvmc_net::prune].
  pub prune: bool,

  /// Enables [hvmc_net::pre_reduce].
  pub pre_reduce: bool,

  /// Enables [term::transform::detach_supercombinators].
  pub supercombinators: bool,

  /// Enables [term::transform::simplify_main_ref].
  pub simplify_main: bool,

  /// Enables dereferences in [hvmc_net::pre_reduce] pass.
  pub pre_reduce_refs: bool,

  /// Enables [term::transform::definition_merge]
  pub merge_definitions: bool,

  pub lazy: bool
}

impl Opts {
  /// All optimizations enabled.
  pub fn heavy() -> Self {
    Self {
      eta: true,
      ref_to_ref: true,
      prune: true,
      pre_reduce: true,
      supercombinators: true,
      simplify_main: true,
      pre_reduce_refs: true,
      merge_definitions: true,
      lazy: false
    }
  }

  /// All optimizations disabled, except detach supercombinators.
  pub fn light() -> Self {
    Self { supercombinators: true, ..Self::default() }
  }
}

impl Opts {
  pub fn check(&self) {
    if !self.supercombinators {
      println!(
        "Warning: Running in strict mode without enabling the supercombinators pass can lead to some functions expanding infinitely."
      );
    }
  }
}

#[derive(Default, Clone, Copy)]
pub struct WarningOpts {
  pub match_only_vars: WarnState,
  pub unused_defs: WarnState,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum WarnState {
  #[default]
  Warn,
  Allow,
  Deny,
}

impl WarningOpts {
  pub fn allow_all() -> Self {
    Self { match_only_vars: WarnState::Allow, unused_defs: WarnState::Allow }
  }

  pub fn deny_all() -> Self {
    Self { match_only_vars: WarnState::Deny, unused_defs: WarnState::Deny }
  }

  pub fn warn_all() -> Self {
    Self { match_only_vars: WarnState::Warn, unused_defs: WarnState::Warn }
  }

  /// Filters warnings based on the enabled flags.
  pub fn filter<'a>(&'a self, wrns: &'a [Warning], ws: WarnState) -> Vec<&Warning> {
    wrns
      .iter()
      .filter(|w| {
        (match w {
          Warning::MatchOnlyVars { .. } => self.match_only_vars,
          Warning::UnusedDefinition { .. } => self.unused_defs,
        }) == ws
      })
      .collect()
  }
}

/// Either just prints warnings or returns Err when any denied was produced.
pub fn display_warnings(warning_opts: WarningOpts, warnings: &[Warning]) -> Result<(), String> {
  let warns = warning_opts.filter(warnings, WarnState::Warn);
  if !warns.is_empty() {
    let warns = warns.iter().join("\n");
    eprintln!("Warnings:\n{warns}");
  }
  let denies = warning_opts.filter(warnings, WarnState::Deny);
  if !denies.is_empty() {
    let denies = denies.iter().join("\n");
    return Err(format!("{denies}\nCould not run the code because of the previous warnings"));
  }
  Ok(())
}

pub struct CompileResult {
  pub core_book: hvmc::ast::Book,
  pub hvmc_names: HvmcNames,
  pub labels: Labels,
  pub warnings: Vec<Warning>,
}

impl std::fmt::Debug for CompileResult {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for warn in &self.warnings {
      writeln!(f, "// WARNING: {}", warn)?;
    }
    write!(f, "{}", show_book(&self.core_book))
  }
}

pub enum Warning {
  MatchOnlyVars { def_name: Name },
  UnusedDefinition { def_name: Name },
}

impl std::fmt::Display for Warning {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Warning::MatchOnlyVars { def_name } => {
        write!(f, "Match expression at definition '{def_name}' only uses var patterns.")
      }
      Warning::UnusedDefinition { def_name } => write!(f, "Unused definition '{def_name}'."),
    }
  }
}

pub struct RunInfo {
  pub stats: RunStats,
  pub readback_errors: ReadbackErrors,
  pub net: Net,
}

pub struct RunStats {
  pub rewrites: Rewrites,
  pub used: usize,
  pub run_time: f64,
}

fn expand(net: &mut hvmc::run::Net, book: &hvmc::run::Book) {
  match net {
    hvmc::run::Net::Eager(net) => net.net.expand(book),
    _ => {}
  }
}

fn reduce(net: &mut hvmc::run::Net, book: &hvmc::run::Book, limit: usize) -> usize {
  match net {
    hvmc::run::Net::Eager(net) => net.net.reduce(book, limit),
    _ => panic!("Unsupported configuration, disable debug mode `-D` or enable less optimizations `-O=0`"),
  }
}

fn rdex(net: &mut hvmc::run::Net) -> &mut Vec<(hvmc::run::Ptr, hvmc::run::Ptr)> {
  match net {
    hvmc::run::Net::Lazy(net) => &mut net.net.rdex,
    hvmc::run::Net::Eager(net) => &mut net.net.rdex,
  }
}

fn net_from_runtime(net: &hvmc::run::Net) -> Net {
  match net {
    hvmc::run::Net::Lazy(net) => hvmc::ast::net_from_runtime(&net.net),
    hvmc::run::Net::Eager(net) => hvmc::ast::net_from_runtime(&net.net),
  }
}

fn net_to_runtime(rt_net: &mut hvmc::run::Net, net: &Net) {
  match rt_net {
    hvmc::run::Net::Lazy(rt_net) => hvmc::ast::net_to_runtime(&mut rt_net.net, net),
    hvmc::run::Net::Eager(rt_net) => hvmc::ast::net_to_runtime(&mut rt_net.net, net),
  }
}

fn runtime_net_to_runtime_def(net: &hvmc::run::Net) -> Def {
  match net {
    hvmc::run::Net::Lazy(net) => hvmc::ast::runtime_net_to_runtime_def(&net.net),
    hvmc::run::Net::Eager(net) => hvmc::ast::runtime_net_to_runtime_def(&net.net),
  }
}
