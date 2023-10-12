#![feature(lazy_cell)]
#![feature(box_patterns)]

pub mod ast;
pub mod from_core;
pub mod net;
pub mod semantic;
pub mod term;
pub mod to_core;

use ast::{DefId, DefinitionBook, Term};
use from_core::readback_net;
use hvmc::{readback_lnet, LBook, LNet};
use semantic::check_main;
use std::time::Instant;
use term::DefNames;
use to_core::{book_to_hvm_core, book_to_hvm_internal};

pub use crate::term::load_book::load_file_to_book;

pub fn check_book(mut book: DefinitionBook) -> anyhow::Result<()> {
  // TODO: Do the checks without having to do full compilation
  compile_book(&mut book)?;
  Ok(())
}

pub fn compile_book(book: &mut DefinitionBook) -> anyhow::Result<LBook> {
  book.check_rule_arities()?;
  book.flatten_rules();
  book.sanitize_vars()?;
  book.detach_supercombinators();
  // book.try_into_affine()?;
  let core_book = book_to_hvm_core(book)?;
  Ok(core_book)
}

pub fn run_compiled(book: &LBook, main: DefId, mem_size: usize) -> (LNet, RunStats) {
  let (mut root, runtime_book) = book_to_hvm_internal(book, main, mem_size);
  let start_time = Instant::now();
  // Computes its normal form
  root.normal(&runtime_book);
  let elapsed = start_time.elapsed().as_secs_f64();
  let rewrites = Rewrites { anni: root.anni, comm: root.comm, eras: root.eras, dref: root.dref };
  let net = readback_lnet(&root);
  let def = root.to_def();
  let stats = RunStats { rewrites, used: def.node.len(), run_time: elapsed };
  (net, stats)
}

pub fn run_book(mut book: DefinitionBook, mem_size: usize) -> anyhow::Result<(Term, DefNames, RunInfo)> {
  let main = check_main(&book)?;
  let compiled = compile_book(&mut book)?;
  let (res_lnet, stats) = run_compiled(&compiled, main, mem_size);
  let (res_term, valid_readback) = readback_net(&res_lnet, &book)?;
  let info = RunInfo { stats, valid_readback, lnet: res_lnet };
  Ok((res_term, book.def_names, info))
}

pub struct RunInfo {
  pub stats: RunStats,
  pub valid_readback: bool,
  pub lnet: LNet,
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
