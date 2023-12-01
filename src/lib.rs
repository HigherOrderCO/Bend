#![feature(box_patterns)]

use hvmc::{
  ast::{book_to_runtime, name_to_val, net_from_runtime, runtime_net_to_runtime_def, show_book, Net},
  run::{Heap, Rewrites, Val},
};
use hvmc_net::pre_reduce::pre_reduce_book;
use net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::nets_to_hvmc};
use std::{collections::HashMap, time::Instant};
use term::{book_to_nets, net_to_term::net_to_term_non_linear, Book, DefId, DefNames, Name, Term};

pub mod hvmc_net;
pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

pub fn check_book(mut book: Book) -> Result<(), String> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book)?;
  Ok(())
}

pub enum Warning {
  TooManyDups { name: String },
}

impl std::fmt::Display for Warning {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let Self::TooManyDups { name } = self;
    write!(f, "This rule generated more dups then what is supported by hvm-core: {}", name)
  }
}

pub struct CompileResult {
  pub core_book: hvmc::ast::Book,
  pub hvmc_name_to_id: HashMap<Val, DefId>,
  pub labels_to_tag: HashMap<u32, Name>,
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

pub fn compile_book(book: &mut Book) -> Result<CompileResult, String> {
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.resolve_ctrs_in_pats();
  book.generate_scott_adts();
  book.check_unbound_pats()?;
  book.flatten_rules();
  let def_types = book.infer_def_types()?;
  book.check_exhaustive_patterns(&def_types)?;
  book.encode_pattern_matching_functions(&def_types);
  book.resolve_refs();
  book.simplify_matches()?;
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars();
  book.eta_reduction();
  book.detach_supercombinators();
  book.simplify_ref_to_ref()?;
  book.prune(main);
  let (nets, id_to_hvmc_name, labels_to_tag, warnings) = book_to_nets(book, main);
  let mut core_book = nets_to_hvmc(nets, &id_to_hvmc_name)?;
  pre_reduce_book(&mut core_book)?;
  let hvmc_name_to_id = id_to_hvmc_name.into_iter().map(|(k, v)| (v, k)).collect();
  Ok(CompileResult { core_book, hvmc_name_to_id, labels_to_tag, warnings })
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
    while root.rdex.len() > 0 {
      hook(&net_from_runtime(&root));
      root.reduce(&runtime_book, 1);
      root.expand(&runtime_book);
    }
  } else {
    if parallel {
      root.parallel_normal(&runtime_book);
    } else {
      root.normal(&runtime_book)
    }
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
) -> Result<(Term, DefNames, RunInfo), String> {
  let CompileResult { core_book, hvmc_name_to_id, labels_to_tag, warnings } = compile_book(&mut book)?;

  if !warnings.is_empty() {
    for warn in warnings {
      eprintln!("{}", warn);
    }

    return Err("Could not run the code because of the previous warnings".into());
  }

  let debug_hook = if debug {
    Some(|net: &_| debug_hook(net, &book, &hvmc_name_to_id, &labels_to_tag))
  } else {
    None
  };
  let (res_lnet, stats) = run_compiled(&core_book, mem_size, parallel, debug_hook);
  let net = hvmc_to_net(&res_lnet, &|val| hvmc_name_to_id[&val]);
  let (res_term, valid_readback) = net_to_term_non_linear(&net, &book, &labels_to_tag);
  let info = RunInfo { stats, valid_readback, net: res_lnet };
  Ok((res_term, book.def_names, info))
}

pub fn desugar_book(book: &mut Book) -> Result<(), String> {
  let main = book.check_has_main()?;
  book.check_shared_names()?;
  book.resolve_ctrs_in_pats();
  book.generate_scott_adts();
  book.check_unbound_pats()?;
  book.flatten_rules();
  let def_types = book.infer_def_types()?;
  book.check_exhaustive_patterns(&def_types)?;
  book.encode_pattern_matching_functions(&def_types);
  book.resolve_refs();
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars();
  book.eta_reduction();
  book.detach_supercombinators();
  book.simplify_ref_to_ref()?;
  book.prune(main);
  Ok(())
}

fn debug_hook(
  net: &Net,
  book: &Book,
  hvmc_name_to_id: &HashMap<u64, DefId>,
  labels_to_tag: &HashMap<u32, Name>,
) {
  let net = hvmc_to_net(&net, &|val| hvmc_name_to_id[&val]);
  let (res_term, valid_readback) = net_to_term_non_linear(&net, &book, &labels_to_tag);
  println!(
    "{}{}\n---------------------------------------",
    if valid_readback { "" } else { "[invalid] " },
    res_term.to_string(&book.def_names)
  );
}

pub struct RunInfo {
  pub stats: RunStats,
  pub valid_readback: bool,
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
