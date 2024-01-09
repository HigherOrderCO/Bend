#![feature(box_patterns)]

use hvmc::{
  ast::{book_to_runtime, name_to_val, net_from_runtime, runtime_net_to_runtime_def, show_book, Net},
  run::{Heap, Rewrites},
};
use hvmc_net::pre_reduce::pre_reduce_book;
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

use crate::term::parser::parser::{parse_repl, Repl};

pub fn check_book(mut book: Book) -> Result<(), String> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book, OptimizationLevel::Light, true)?;
  Ok(())
}

pub enum Warning {}

impl std::fmt::Display for Warning {
  fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match *self {}
  }
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

pub fn compile_book(
  book: &mut Book,
  opt_level: OptimizationLevel,
  prune: bool,
) -> Result<CompileResult, String> {
  let main = desugar_book(book, opt_level, prune)?;
  let (nets, hvmc_names, labels) = book_to_nets(book, main);
  let mut core_book = nets_to_hvmc(nets, &hvmc_names)?;
  pre_reduce_book(&mut core_book, opt_level >= OptimizationLevel::Heavy)?;
  Ok(CompileResult { core_book, hvmc_names, labels, warnings: vec![] })
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

pub fn run_book(
  mut book: Book,
  mem_size: usize,
  parallel: bool,
  debug: bool,
  linear: bool,
  opt_level: OptimizationLevel,
) -> Result<(Term, DefNames, RunInfo), String> {
  let CompileResult { core_book, hvmc_names, labels, warnings } = compile_book(&mut book, opt_level, true)?;

  if !warnings.is_empty() {
    for warn in warnings {
      eprintln!("{}", warn);
    }

    return Err("Could not run the code because of the previous warnings".into());
  }

  let debug_hook =
    if debug { Some(|net: &_| debug_hook(net, &book, &hvmc_names, &labels, linear)) } else { None };
  let (res_lnet, stats) = run_compiled(&core_book, mem_size, parallel, debug_hook);
  let net = hvmc_to_net(&res_lnet, &|id| hvmc_names.hvmc_name_to_id[&id]);
  let (res_term, readback_errors) = net_to_term(&net, &book, &labels, linear);
  let info = RunInfo { stats, readback_errors, net: res_lnet };
  Ok((res_term, book.def_names, info))
}

pub fn run_repl(book: &mut Book) -> Result<(), String> {
  use std::io::Write;

  let CompileResult { mut core_book, mut hvmc_names, mut labels, warnings: _ } =
    compile_book(book, OptimizationLevel::Light, false)?;

  loop {
    std::io::stdout().write(b"*> ").unwrap();
    std::io::stdout().flush().unwrap();
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();

    match parse_repl(&input) {
      Ok(repl) => match repl {
        Repl::Exit => break,
        Repl::Def((name, mut term)) => {
          // TODO: term.desugar method
          term.desugar(&book, Flags::new().set(Term::UNIQUE).set(Term::ETA));

          let def_id = book.def_names.insert(name.clone());
          let name_as_val = name_to_val(&name.0);
          hvmc_names.insert(def_id, name_as_val);

          let compat_net = term::term_to_net::term_to_compat_net(&term, &mut labels);
          let hvmc_net = net::net_to_hvmc::net_to_hvmc(&compat_net, &|id| hvmc_names.id_to_hvmc_name[&id])?;

          core_book.insert(name.0, hvmc_net);
        }
        Repl::Term(mut term) => {
          // TODO: term.desugar method
          term.desugar(&book, Flags::new());

          let def_id = hvmc::u60::new(u64::MAX);
          let compat_net = term::term_to_net::term_to_compat_net(&term, &mut labels);
          let hvmc_net = net::net_to_hvmc::net_to_hvmc(&compat_net, &|id| hvmc_names.id_to_hvmc_name[&id])?;

          let mut runtime_book = hvmc::ast::book_to_runtime(&core_book);

          let heap = hvmc::run::Heap::init(1 << 16);
          let mut runtime_net = hvmc::run::Net::new(&heap);
          hvmc::ast::net_to_runtime(&mut runtime_net, &hvmc_net);
          runtime_book.def(def_id, hvmc::ast::runtime_net_to_runtime_def(&runtime_net));

          let heap = hvmc::run::Heap::init(1 << 16);
          let mut root = hvmc::run::Net::new(&heap);
          root.boot(def_id);
          root.parallel_normal(&runtime_book);

          let result = hvmc::ast::net_from_runtime(&root);
          let net = net::hvmc_to_net::hvmc_to_net(&result, &|id| hvmc_names.hvmc_name_to_id[&id]);
          let (term, errors) = term::net_to_term(&net, book, &labels, false);

          if errors.is_empty() {
            println!("{}", term.display(&book.def_names));
          } else {
            format!("Invalid readback: {:?}\n", errors);
          }
        }
      },
      Err(e) => {
        let msg = itertools::Itertools::join(&mut e.into_iter().map(|e| e.to_string()), "\n");
        println!("{msg}")
      }
    }
  }
  Ok(())
}

pub fn desugar_book(book: &mut Book, opt_level: OptimizationLevel, prune: bool) -> Result<DefId, String> {
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.encode_strs();
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
  book.simplify_ref_to_ref()?;
  if prune {
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

fn debug_hook(net: &Net, book: &Book, hvmc_names: &HvmcNames, labels: &Labels, linear: bool) {
  let net = hvmc_to_net(net, &|id| hvmc_names.hvmc_name_to_id[&id]);
  let (res_term, errors) = net_to_term(&net, book, labels, linear);
  println!(
    "{}{}\n---------------------------------------",
    if errors.is_empty() { "".to_string() } else { format!("Invalid readback: {:?}\n", errors) },
    res_term.display(&book.def_names)
  );
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

pub fn total_rewrites(rwrts: &Rewrites) -> usize {
  rwrts.anni + rwrts.comm + rwrts.eras + rwrts.dref + rwrts.oper
}

struct Flags(u8);

impl Flags {
  fn new() -> Self {
    Self(0)
  }

  fn set(self, flag: u8) -> Self {
    Self(self.0 | flag)
  }

  fn has(&self, flag: u8) -> bool {
    self.0 & flag == flag
  }
}

impl Term {
  const UNIQUE: u8 = 1;
  const ETA: u8 = 1 << 1;

  fn desugar(&mut self, book: &Book, flags: Flags) {
    if flags.has(Term::UNIQUE) {
      self.make_var_names_unique();
    }
    self.encode_str();
    self.linearize_vars();
    self.resolve_refs(&book.def_names);
    if flags.has(Term::ETA) {
      self.eta_reduction();
    }
  }
}
