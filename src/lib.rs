#![feature(lazy_cell)]
#![feature(box_patterns)]

use hvmc::ast::{book_to_runtime, net_from_runtime, Book, Net};
use net::{core_net_to_compat, nets_to_hvm_core};
use std::time::Instant;
use term::{book_to_compact_nets, readback_compat, DefId, DefNames, DefinitionBook, Term};

pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

pub fn check_book(mut book: DefinitionBook) -> anyhow::Result<()> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book)?;
  Ok(())
}

pub fn compile_book(book: &mut DefinitionBook) -> anyhow::Result<Book> {
  book.resolve_refs();
  book.check_unbound_vars()?;
  book.make_var_names_unique();
  book.linearize_vars()?;
  book.check_self_referential_defs()?;
  book.detach_supercombinators();
  let nets = book_to_compact_nets(book)?;
  let core_book = nets_to_hvm_core(nets)?;
  Ok(core_book)
}

pub fn run_compiled(book: &Book, main: DefId, mem_size: usize) -> (Net, RunStats) {
  let runtime_book = book_to_runtime(book);
  let mut root = hvmc::run::Net::new(mem_size);
  root.boot(main.to_internal());

  let start_time = Instant::now();

  // Computes its normal form
  root.normal(&runtime_book);

  let elapsed = start_time.elapsed().as_secs_f64();
  let rewrites = Rewrites { anni: root.anni, comm: root.comm, eras: root.eras, dref: root.dref };
  let net = net_from_runtime(&root);
  let def = root.to_def();
  let stats = RunStats { rewrites, used: def.node.len(), run_time: elapsed };
  (net, stats)
}

pub fn run_book(mut book: DefinitionBook, mem_size: usize) -> anyhow::Result<(Term, DefNames, RunInfo)> {
  let main = book.check_has_main()?;
  let compiled = compile_book(&mut book)?;
  let (res_lnet, stats) = run_compiled(&compiled, main, mem_size);
  let compat_net = core_net_to_compat(&res_lnet)?;
  let (res_term, valid_readback) = readback_compat(&compat_net, &book);
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
}

impl Rewrites {
  pub fn total_rewrites(&self) -> usize {
    self.anni + self.comm + self.eras + self.dref
  }
}
