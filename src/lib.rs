#![feature(lazy_cell)]

pub mod ast;
pub mod from_core;
pub mod loader;
pub mod parser;
pub mod semantic;
pub mod to_core;

use ast::{core::Book, hvm_lang::DefNames, DefinitionBook, Term};
use from_core::readback_net;
use hvm_core::{readback_lnet, LNet};
use semantic::check_main;
use std::time::Instant;
use to_core::{book_to_hvm_core, book_to_hvm_internal};

pub use loader::load_file_to_book;

pub fn check_book(book: DefinitionBook) -> anyhow::Result<()> {
  // TODO: Do the checks without having to do full compilation
  compile_book(book)?;
  Ok(())
}

pub fn compile_book(mut book: DefinitionBook) -> anyhow::Result<(Book, DefNames)> {
  // book.check_rule_arities()?;
  book.sanitize_vars()?;
  // book.try_into_affine()?;
  let core_book = book_to_hvm_core(&book)?;
  Ok((core_book, book.def_names))
}

pub fn run_compiled(book: &Book, mem_size: usize) -> anyhow::Result<(LNet, RunStats)> {
  let (mut root, runtime_book) = book_to_hvm_internal(book, mem_size)?;

  let start_time = Instant::now();

  // Computes its normal form
  root.normal(&runtime_book);

  let elapsed = start_time.elapsed().as_secs_f64();

  let rewrites = Rewrites { anni: root.anni, comm: root.comm, eras: root.eras, dref: root.dref };
  let def = root.clone().to_def();
  let stats = RunStats { rewrites, used: def.node.len(), run_time: elapsed };
  // TODO: Make readback for hvm-core Def type
  let net = readback_lnet(&root);
  Ok((net, stats))
}

pub fn run_book(book: DefinitionBook, mem_size: usize) -> anyhow::Result<(Term, DefNames, RunInfo)> {
  check_main(&book)?;
  let (compiled, def_names) = compile_book(book)?;
  let (res_lnet, stats) = run_compiled(&compiled, mem_size)?;
  let (res_term, valid_readback) = readback_net(&res_lnet)?;
  let info = RunInfo { stats, valid_readback, lnet: res_lnet };
  Ok((res_term, def_names, info))
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
