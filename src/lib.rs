#![feature(box_patterns)]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

use hvmc::{
  ast::{book_to_runtime, show_book, Net},
  run::{Def, Rewrites},
};
use hvmc_net::{pre_reduce::pre_reduce_book, prune::prune_defs};
use net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::nets_to_hvmc};
use std::time::Instant;
use term::{
  book_to_nets, net_to_term,
  term_to_net::{HvmcNames, Labels},
  Book, DefId, DefNames, ReadbackError, Term,
};

pub mod hvmc_net;
pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

pub fn check_book(mut book: Book) -> Result<(), String> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book, OptimizationLevel::Light, false)?;
  Ok(())
}

pub fn compile_book(
  book: &mut Book,
  opt_level: OptimizationLevel,
  lazy: bool,
) -> Result<CompileResult, String> {
  let main = desugar_book(book, opt_level)?;
  let (nets, hvmc_names, labels) = book_to_nets(book, main);
  let mut core_book = nets_to_hvmc(nets, &hvmc_names)?;
  pre_reduce_book(&mut core_book, opt_level >= OptimizationLevel::Heavy, lazy)?;
  if opt_level >= OptimizationLevel::Heavy {
    prune_defs(&mut core_book);
  }
  Ok(CompileResult { core_book, hvmc_names, labels, warnings: vec![] })
}

pub fn desugar_book(book: &mut Book, opt_level: OptimizationLevel) -> Result<DefId, String> {
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.encode_strs()?;
  book.encode_lists()?;
  book.generate_scott_adts();
  book.resolve_refs();
  encode_pattern_matching(book)?;
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars();
  if opt_level >= OptimizationLevel::Heavy {
    book.eta_reduction();
  }
  book.detach_supercombinators();
  if opt_level >= OptimizationLevel::Heavy {
    book.simplify_ref_to_ref()?;
  }
  book.simplify_main_ref(main);
  if opt_level >= OptimizationLevel::Heavy {
    book.prune(main);
  }
  Ok(main)
}

pub fn encode_pattern_matching(book: &mut Book) -> Result<(), String> {
  book.resolve_ctrs_in_pats();
  book.check_unbound_pats()?;
  book.desugar_let_destructors();
  book.desugar_implicit_match_binds();
  book.extract_matches()?;
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
  lazy: bool,
  opt_level: OptimizationLevel,
) -> Result<(Term, DefNames, RunInfo), String> {
  let CompileResult { core_book, hvmc_names, labels, warnings } = compile_book(&mut book, opt_level, lazy)?;

  if !warnings.is_empty() {
    for warn in warnings {
      eprintln!("{}", warn);
    }
    return Err("Could not run the code because of the previous warnings".into());
  }

  fn debug_hook(net: &Net, book: &Book, hvmc_names: &HvmcNames, labels: &Labels, linear: bool) {
    let net = hvmc_to_net(net, &|id| hvmc_names.hvmc_name_to_id[&id]);
    let (res_term, errors) = net_to_term(&net, book, labels, linear);
    println!(
      "{}{}\n---------------------------------------",
      if errors.is_empty() { "".to_string() } else { format!("Invalid readback: {:?}\n", errors) },
      res_term.display(&book.def_names)
    );
  }
  let debug_hook =
    if debug { Some(|net: &_| debug_hook(net, &book, &hvmc_names, &labels, linear)) } else { None };

  let (res_lnet, stats) = run_compiled(&core_book, mem_size, parallel, lazy, debug_hook);
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
    root.normal(&runtime_book)
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

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum OptimizationLevel {
  /// The minimum amount of transformations to produce valid hvmc outputs.
  Light,
  /// More aggressive optimizations.
  Heavy,
}

impl From<usize> for OptimizationLevel {
  fn from(value: usize) -> Self {
    if value == 0 { OptimizationLevel::Light } else { OptimizationLevel::Heavy }
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

pub enum Warning {}

impl std::fmt::Display for Warning {
  fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {}
  }
}

pub struct RunInfo {
  pub stats: RunStats,
  pub readback_errors: Vec<ReadbackError>,
  pub net: Net,
}

pub struct RunStats {
  pub rewrites: Rewrites,
  pub used: usize,
  pub run_time: f64,
}

fn expand(net: &mut hvmc::run::Net, book: &hvmc::run::Book) {
  match net {
    hvmc::run::Net::Lazy(net) => net.net.expand(book),
    hvmc::run::Net::Eager(net) => net.net.expand(book),
  }
}

fn reduce(net: &mut hvmc::run::Net, book: &hvmc::run::Book, limit: usize) -> usize {
  match net {
    hvmc::run::Net::Lazy(_net) => todo!(), //net.net.reduce(book, 1),
    hvmc::run::Net::Eager(net) => net.net.reduce(book, limit),
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
