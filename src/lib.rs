#![feature(slice_group_by)]

pub mod ast;
pub mod from_core;
pub mod loader;
pub mod parser;
pub mod semantic;
pub mod to_core;

use ast::{core::Book, DefinitionBook, Term};
use from_core::readback_net;
use hvm_core::{readback_lnet, LNet};
use semantic::check_main;
use std::time::Instant;
use to_core::{book_to_hvm_core, book_to_hvm_internal};

pub use loader::load_file_to_book;

pub fn check_book(book: &DefinitionBook) -> anyhow::Result<()> {
  // TODO: Do the checks without having to do full compilation
  compile_book(book)?;
  Ok(())
}

pub fn compile_book(book: &DefinitionBook) -> anyhow::Result<Book> {
  book_to_hvm_core(book)
}

pub fn run_compiled(book: &Book) -> anyhow::Result<(LNet, RunStats)> {
  let (mut root, runtime_book) = book_to_hvm_internal(book)?;

  let start_time = Instant::now();

  // Computes its normal form
  root.normal(&runtime_book);

  let elapsed = start_time.elapsed().as_secs_f64();

  let stats = RunStats { rewrites: root.rwts, size: root.node.len(), used: root.used, run_time: elapsed };
  let net = readback_lnet(&root);
  Ok((net, stats))
}

pub fn run_book(book: &DefinitionBook) -> anyhow::Result<(Term, RunStats)> {
  check_main(book)?;
  let compiled = compile_book(book)?;
  let (res, stats) = run_compiled(&compiled)?;
  let res = readback_net(&res)?;
  Ok((res, stats))
}

pub struct RunStats {
  pub rewrites: usize,
  pub size: usize,
  pub used: usize,
  pub run_time: f64,
}
