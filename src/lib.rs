#![feature(box_patterns)]

use hvmc::{
  ast::{book_to_runtime, name_to_val, net_from_runtime, runtime_net_to_runtime_def, show_book, Net},
  run::{Heap, Rewrites},
};
use hvmc_net::{pre_reduce::pre_reduce_book, prune::prune_defs};
use itertools::Itertools;
use net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::nets_to_hvmc};
use std::time::Instant;
use term::{
  book_to_nets, net_to_term,
  term_to_net::{HvmcNames, Labels},
  transform::{encode_lists::BuiltinList, encode_strs::BuiltinString},
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
  pre_reduce_book(&mut core_book, opts.pre_reduce)?;
  if opts.prune {
    prune_defs(&mut core_book);
  }
  Ok(CompileResult { core_book, hvmc_names, labels, warnings })
}

pub fn desugar_book(
  book: &mut Book,
  Opts { eta, ref_to_ref, prune, supercombinators, simplify_main, .. }: Opts,
) -> Result<(DefId, Vec<Warning>), String> {
  let mut warnings = Vec::new();
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.encode_builtin_adt(BuiltinString)?;
  book.encode_builtin_adt(BuiltinList)?;
  book.generate_scott_adts();
  book.resolve_refs()?;
  encode_pattern_matching(book, &mut warnings)?;
  // sanity check
  book.check_unbound_vars()?;
  book.normalize_native_matches()?;
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars();
  book.eta_reduction(eta);
  // sanity check
  book.check_unbound_vars()?;
  if supercombinators {
    book.detach_supercombinators(main);
  }
  if ref_to_ref {
    book.simplify_ref_to_ref()?;
  }
  if simplify_main {
    book.simplify_main_ref(main);
  }
  if prune {
    book.prune(main);
  }
  Ok((main, warnings))
}

pub fn encode_pattern_matching(book: &mut Book, warnings: &mut Vec<Warning>) -> Result<(), String> {
  book.resolve_ctrs_in_pats();
  book.check_unbound_pats()?;
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
  skip_warnings: bool,
  opts: Opts,
) -> Result<(Term, DefNames, RunInfo), String> {
  let CompileResult { core_book, hvmc_names, labels, warnings } = compile_book(&mut book, opts)?;

  if !warnings.is_empty() {
    let warnings = warnings.iter().join("\n");
    if !skip_warnings {
      return Err(format!("{warnings}\nCould not run the code because of the previous warnings"));
    } else {
      eprintln!("{warnings}");
    }
  }

  fn debug_hook(net: &Net, book: &Book, hvmc_names: &HvmcNames, labels: &Labels, linear: bool) {
    let net = hvmc_to_net(net, &|id| hvmc_names.hvmc_name_to_id[&id]);
    let (res_term, errors) = net_to_term(&net, book, labels, linear);
    println!(
      "{}{}\n---------------------------------------",
      if errors.is_empty() {
        "".to_string()
      } else {
        format!("Invalid readback:\n{}\n", errors.display(&book.def_names))
      },
      res_term.display(&book.def_names)
    );
  }
  let debug_hook =
    if debug { Some(|net: &_| debug_hook(net, &book, &hvmc_names, &labels, linear)) } else { None };

  let (res_lnet, stats) = run_compiled(&core_book, mem_size, parallel, debug_hook);
  let net = hvmc_to_net(&res_lnet, &|id| hvmc_names.hvmc_name_to_id[&id]);
  let (res_term, readback_errors) = net_to_term(&net, &book, &labels, linear);
  let info = RunInfo { stats, readback_errors, net: res_lnet };
  Ok((res_term, book.def_names, info))
}

pub fn run_compiled(
  book: &hvmc::ast::Book,
  mem_size: usize,
  parallel: bool,
  hook: Option<impl FnMut(&Net)>,
) -> (Net, RunStats) {
  let runtime_book = book_to_runtime(book);
  let heap = Heap::init(mem_size);
  let mut root = hvmc::run::Net::new(&heap);
  root.boot(name_to_val(DefNames::ENTRY_POINT));

  let start_time = Instant::now();

  if let Some(mut hook) = hook {
    root.expand(&runtime_book);
    while !root.rdex.is_empty() {
      hook(&net_from_runtime(&root));
      root.reduce(&runtime_book, 1);
      root.expand(&runtime_book);
    }
  } else if parallel {
    root.parallel_normal(&runtime_book);
  } else {
    root.normal(&runtime_book)
  }

  let elapsed = start_time.elapsed().as_secs_f64();

  let net = net_from_runtime(&root);
  let def = runtime_net_to_runtime_def(&root);
  let stats = RunStats { rewrites: root.rwts, used: def.node.len(), run_time: elapsed };
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
    }
  }

  /// All optimizations disabled, except detach supercombinators.
  pub fn light() -> Self {
    Self { supercombinators: true, ..Self::default() }
  }
}

impl Opts {
  pub fn from_vec(&mut self, values: Vec<String>) -> Result<(), String> {
    for value in values {
      match value.as_ref() {
        "all" => *self = Opts::heavy(),
        "no-all" => *self = Opts::default(),
        "eta" => self.eta = true,
        "no-eta" => self.eta = false,
        "prune" => self.prune = true,
        "no-prune" => self.prune = false,
        "ref-to-ref" => self.ref_to_ref = true,
        "no-ref-to-ref" => self.ref_to_ref = false,
        "pre-reduce" => self.pre_reduce = true,
        "no-pre-reduce" => self.pre_reduce = false,
        "supercombinators" => self.supercombinators = true,
        "no-supercombinators" => self.supercombinators = false,
        "simplify-main" => self.simplify_main = true,
        "no-simplify-main" => self.simplify_main = false,
        other => return Err(format!("Unknown option '{other}'.")),
      }
    }
    Ok(())
  }
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
}

impl std::fmt::Display for Warning {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Warning::MatchOnlyVars { def_name } => {
        write!(f, "Match expression at definition '{def_name}' only uses var patterns.")
      }
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
