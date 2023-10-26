#![feature(box_patterns)]

use hvmc::{
  ast::{book_to_runtime, name_to_val, net_from_runtime, show_net, Net},
  run::Val,
};
use net::{hvmc_to_net, nets_to_hvm_core};
use std::{collections::HashMap, time::Instant};
use term::{book_to_nets, net_to_term::net_to_term_non_linear, Book, DefId, DefNames, Term};

pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

pub fn check_book(mut book: Book) -> anyhow::Result<()> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book)?;
  Ok(())
}

pub fn compile_book(book: &mut Book) -> anyhow::Result<(hvmc::ast::Book, HashMap<Val, DefId>)> {
  let main = book.check_has_main()?;
  book.resolve_refs();
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars()?;
  book.check_ref_to_ref()?;
  book.detach_supercombinators();
  book.prune(main);
  let (nets, id_to_hvmc_name) = book_to_nets(book, main)?;
  let core_book = nets_to_hvm_core(nets, &id_to_hvmc_name)?;
  let hvmc_name_to_id = id_to_hvmc_name.into_iter().map(|(k, v)| (v, k)).collect();
  Ok((core_book, hvmc_name_to_id))
}

pub fn run_compiled(book: &hvmc::ast::Book, mem_size: usize) -> (Net, RunStats) {
  let runtime_book = book_to_runtime(book);
  let mut root = hvmc::run::Net::new(mem_size);
  root.boot(name_to_val(DefNames::ENTRY_POINT));

  let start_time = Instant::now();
  root.normal(&runtime_book);
  let elapsed = start_time.elapsed().as_secs_f64();
  let rewrites =
    Rewrites { anni: root.anni, comm: root.comm, eras: root.eras, dref: root.dref, oper: root.oper };
  let net = net_from_runtime(&root);
  eprintln!("{}", show_net(&net));
  let def = root.to_def();
  let stats = RunStats { rewrites, used: def.node.len(), run_time: elapsed };
  (net, stats)
}

pub fn run_book(mut book: Book, mem_size: usize) -> anyhow::Result<(Term, DefNames, RunInfo)> {
  let (compiled, hvmc_name_to_id) = compile_book(&mut book)?;
  let (res_lnet, stats) = run_compiled(&compiled, mem_size);
  let net = hvmc_to_net(&res_lnet, &|val| hvmc_name_to_id[&val])?;
  let (res_term, valid_readback) = net_to_term_non_linear(&net, &book);
  let info = RunInfo { stats, valid_readback, net: res_lnet };
  Ok((res_term, book.def_names, info))
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

pub struct Rewrites {
  pub anni: usize,
  pub comm: usize,
  pub eras: usize,
  pub dref: usize,
  pub oper: usize,
}

impl Rewrites {
  pub fn total_rewrites(&self) -> usize {
    self.anni + self.comm + self.eras + self.dref + self.oper
  }
}
