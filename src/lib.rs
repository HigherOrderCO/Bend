use crate::{
  fun::{book_to_hvm, net_to_term::net_to_term, term_to_net::Labels, Book, Ctx, Term},
  hvm::{
    add_recursive_priority::add_recursive_priority,
    check_net_size::{check_net_sizes, MAX_NET_SIZE_CUDA},
    eta_reduce::eta_reduce_hvm_net,
    hvm_book_show_pretty,
    inline::inline_hvm_book,
    mutual_recursion,
    prune::prune_hvm_book,
  },
};
use diagnostics::{Diagnostics, DiagnosticsConfig, ERR_INDENT_SIZE};
use net::hvm_to_net::hvm_to_net;

pub mod diagnostics;
// `Name` triggers this warning, but it's safe because we're not using its internal mutability.
#[allow(clippy::mutable_key_type)]
pub mod fun;
pub mod hvm;
pub mod imp;
pub mod imports;
pub mod net;
mod utils;

pub use fun::load_book::{load_file_to_book, load_to_book};

pub const ENTRY_POINT: &str = "main";
pub const HVM1_ENTRY_POINT: &str = "Main";
pub const HVM_OUTPUT_END_MARKER: &str = "Result: ";

pub fn check_book(
  book: &mut Book,
  diagnostics_cfg: DiagnosticsConfig,
  compile_opts: CompileOpts,
) -> Result<Diagnostics, Diagnostics> {
  // TODO: Do the checks without having to do full compilation
  let res = compile_book(book, compile_opts, diagnostics_cfg, None)?;
  Ok(res.diagnostics)
}

pub fn compile_book(
  book: &mut Book,
  opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
) -> Result<CompileResult, Diagnostics> {
  let mut diagnostics = desugar_book(book, opts.clone(), diagnostics_cfg, args)?;

  let (mut hvm_book, labels) = book_to_hvm(book, &mut diagnostics)?;

  if opts.eta {
    hvm_book.defs.values_mut().for_each(eta_reduce_hvm_net);
  }

  mutual_recursion::check_cycles(&hvm_book, &mut diagnostics)?;

  if opts.eta {
    hvm_book.defs.values_mut().for_each(eta_reduce_hvm_net);
  }

  if opts.inline {
    if let Err(e) = inline_hvm_book(&mut hvm_book) {
      diagnostics.add_book_error(format!("During inlining:\n{:ERR_INDENT_SIZE$}{}", "", e));
    }
    diagnostics.fatal(())?;
  }

  if opts.prune {
    let prune_entrypoints = vec![book.hvm_entrypoint().to_string()];
    prune_hvm_book(&mut hvm_book, &prune_entrypoints);
  }

  if opts.check_net_size {
    check_net_sizes(&hvm_book, &mut diagnostics, &opts.target_architecture)?;
  }

  add_recursive_priority(&mut hvm_book);

  Ok(CompileResult { hvm_book, labels, diagnostics })
}

pub fn desugar_book(
  book: &mut Book,
  opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
) -> Result<Diagnostics, Diagnostics> {
  let mut ctx = Ctx::new(book, diagnostics_cfg);

  ctx.check_shared_names();

  ctx.set_entrypoint();

  ctx.book.encode_adts(opts.adt_encoding);

  ctx.fix_match_defs()?;

  ctx.apply_args(args)?;

  ctx.desugar_open()?;

  ctx.book.encode_builtins();

  ctx.resolve_refs()?;

  ctx.desugar_match_defs()?;

  ctx.fix_match_terms()?;

  ctx.book.lift_local_defs();

  ctx.desugar_bend()?;
  ctx.desugar_fold()?;
  ctx.desugar_with_blocks()?;

  ctx.check_unbound_vars()?;

  // Auto match linearization
  ctx.book.make_var_names_unique();
  ctx.book.desugar_use();

  match opts.linearize_matches {
    OptLevel::Disabled => (),
    OptLevel::Alt => ctx.book.linearize_match_binds(),
    OptLevel::Enabled => ctx.book.linearize_matches(),
  }
  // Manual match linearization
  ctx.book.linearize_match_with();

  if opts.type_check {
    type_check_book(&mut ctx)?;
  }

  ctx.book.encode_matches(opts.adt_encoding);

  // sanity check
  ctx.check_unbound_vars()?;

  ctx.book.make_var_names_unique();
  ctx.book.desugar_use();

  ctx.book.make_var_names_unique();
  ctx.book.linearize_vars();

  // sanity check
  ctx.check_unbound_vars()?;

  if opts.float_combinators {
    ctx.book.float_combinators(MAX_NET_SIZE_CUDA);
  }
  // sanity check
  ctx.check_unbound_refs()?;

  // Optimizing passes
  ctx.prune(opts.prune);
  if opts.merge {
    ctx.book.merge_definitions();
  }

  ctx.book.expand_main();

  ctx.book.make_var_names_unique();

  if !ctx.info.has_errors() {
    Ok(ctx.info)
  } else {
    Err(ctx.info)
  }
}

pub fn type_check_book(ctx: &mut Ctx) -> Result<(), Diagnostics> {
  ctx.check_untyped_terms()?;
  ctx.resolve_type_ctrs()?;
  ctx.type_check()?;
  Ok(())
}

pub fn run_book(
  mut book: Book,
  run_opts: RunOpts,
  compile_opts: CompileOpts,
  diagnostics_cfg: DiagnosticsConfig,
  args: Option<Vec<Term>>,
  cmd: &str,
) -> Result<Option<(Term, String, Diagnostics)>, Diagnostics> {
  let CompileResult { hvm_book: core_book, labels, diagnostics } =
    compile_book(&mut book, compile_opts.clone(), diagnostics_cfg, args)?;

  // TODO: Printing should be taken care by the cli module, but we'd
  // like to print any warnings before running so that the user can
  // cancel the run if a problem is detected.
  eprint!("{diagnostics}");

  let out = run_hvm(&core_book, cmd, &run_opts)?;
  let (net, stats) = parse_hvm_output(&out)?;
  let (term, diags) =
    readback_hvm_net(&net, &book, &labels, run_opts.linear_readback, compile_opts.adt_encoding);

  Ok(Some((term, stats, diags)))
}

pub fn readback_hvm_net(
  net: &crate::hvm::ast::Net,
  book: &Book,
  labels: &Labels,
  linear: bool,
  adt_encoding: AdtEncoding,
) -> (Term, Diagnostics) {
  let mut diags = Diagnostics::default();
  let net = hvm_to_net(net);
  let mut term = net_to_term(&net, book, labels, linear, &mut diags);
  #[allow(clippy::mutable_key_type)] // Safe to allow, we know how `Name` works.
  let recursive_defs = book.recursive_defs();
  term.expand_generated(book, &recursive_defs);
  term.resugar_strings(adt_encoding);
  term.resugar_lists(adt_encoding);
  (term, diags)
}

/// Runs an HVM book by invoking HVM as a subprocess.
fn run_hvm(book: &crate::hvm::ast::Book, cmd: &str, run_opts: &RunOpts) -> Result<String, String> {
  let out_path = ".out.hvm";
  std::fs::write(out_path, hvm_book_show_pretty(book)).map_err(|x| x.to_string())?;
  let mut process = std::process::Command::new(run_opts.hvm_path.clone())
    .arg(cmd)
    .arg(out_path)
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::inherit())
    .spawn()
    .map_err(|e| format!("Failed to start hvm process.\n{e}"))?;

  let child_out = std::mem::take(&mut process.stdout).expect("Failed to attach to hvm output");
  let thread_out = std::thread::spawn(move || filter_hvm_output(child_out, std::io::stdout()));

  let _ = process.wait().expect("Failed to wait on hvm subprocess");
  if let Err(e) = std::fs::remove_file(out_path) {
    eprintln!("Error removing HVM output file. {e}");
  }

  let result = thread_out.join().map_err(|_| "HVM output thread panicked.".to_string())??;
  Ok(result)
}

/// Reads the final output from HVM and separates the extra information.
fn parse_hvm_output(out: &str) -> Result<(crate::hvm::ast::Net, String), String> {
  let Some((result, stats)) = out.split_once('\n') else {
    return Err(format!(
      "Failed to parse result from HVM (unterminated result).\nOutput from HVM was:\n{:?}",
      out
    ));
  };
  let mut p = crate::hvm::ast::CoreParser::new(result);
  let Ok(net) = p.parse_net() else {
    return Err(format!("Failed to parse result from HVM (invalid net).\nOutput from HVM was:\n{:?}", out));
  };
  Ok((net, stats.to_string()))
}

/// Filters the output from HVM, separating user output from the
/// result, used for readback and displaying stats.
///
/// Buffers the output from HVM to try to parse it.
fn filter_hvm_output(
  mut stream: impl std::io::Read + Send,
  mut output: impl std::io::Write + Send,
) -> Result<String, String> {
  let mut capturing = false;
  let mut result = String::new();
  let mut buf = [0u8; 1024];
  loop {
    let num_read = match stream.read(&mut buf) {
      Ok(n) => n,
      Err(e) => {
        eprintln!("{e}");
        break;
      }
    };
    if num_read == 0 {
      break;
    }
    let new_buf = &buf[..num_read];
    // TODO: Does this lead to broken characters if printing too much at once?
    let new_str = String::from_utf8_lossy(new_buf);
    if capturing {
      // Store the result
      result.push_str(&new_str);
    } else if let Some((before, after)) = new_str.split_once(HVM_OUTPUT_END_MARKER) {
      // If result started in the middle of the buffer, print what came before and start capturing.
      if let Err(e) = output.write_all(before.as_bytes()) {
        eprintln!("Error writing HVM output. {e}");
      };
      result.push_str(after);
      capturing = true;
    } else {
      // Otherwise, don't capture anything
      if let Err(e) = output.write_all(new_buf) {
        eprintln!("Error writing HVM output. {e}");
      }
    }
  }

  if capturing {
    Ok(result)
  } else {
    output.flush().map_err(|e| format!("Error flushing HVM output. {e}"))?;
    let msg = "HVM output had no result (An error likely occurred)".to_string();
    Err(msg)
  }
}

#[derive(Clone, Debug)]
pub struct RunOpts {
  pub linear_readback: bool,
  pub pretty: bool,
  pub hvm_path: String,
}

impl Default for RunOpts {
  fn default() -> Self {
    RunOpts { linear_readback: false, pretty: false, hvm_path: "hvm".to_string() }
  }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum OptLevel {
  Disabled,
  #[default]
  Enabled,
  Alt,
}

impl OptLevel {
  pub fn enabled(&self) -> bool {
    !matches!(self, OptLevel::Disabled)
  }

  pub fn is_extra(&self) -> bool {
    matches!(self, OptLevel::Enabled)
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompilerTarget {
  C,
  Cuda,
  Unknown,
}

#[derive(Clone, Debug)]
pub struct CompileOpts {
  /// The Compiler target architecture
  pub target_architecture: CompilerTarget,

  /// Enables [hvm::eta_reduce].
  pub eta: bool,

  /// Enables [fun::transform::definition_pruning] and [hvm::prune].
  pub prune: bool,

  /// Enables [fun::transform::linearize_matches].
  pub linearize_matches: OptLevel,

  /// Enables [fun::transform::float_combinators].
  pub float_combinators: bool,

  /// Enables [fun::transform::definition_merge]
  pub merge: bool,

  /// Enables [hvm::inline].
  pub inline: bool,

  /// Enables [hvm::check_net_size].
  pub check_net_size: bool,

  /// Enables [type_check_book].
  pub type_check: bool,

  /// Determines the encoding of constructors and matches.
  pub adt_encoding: AdtEncoding,
}

impl CompileOpts {
  /// Set all optimizing options as true
  #[must_use]
  pub fn set_all(self) -> Self {
    Self {
      target_architecture: self.target_architecture,
      eta: true,
      prune: true,
      float_combinators: true,
      merge: true,
      linearize_matches: OptLevel::Enabled,
      type_check: true,
      inline: true,
      check_net_size: self.check_net_size,
      adt_encoding: self.adt_encoding,
    }
  }

  /// Set all optimizing options as false
  #[must_use]
  pub fn set_no_all(self) -> Self {
    Self {
      target_architecture: self.target_architecture,
      eta: false,
      prune: false,
      linearize_matches: OptLevel::Disabled,
      float_combinators: false,
      merge: false,
      inline: false,
      type_check: self.type_check,
      check_net_size: self.check_net_size,
      adt_encoding: self.adt_encoding,
    }
  }

  pub fn check_for_strict(&self) {
    if !self.float_combinators {
      println!(
        "Warning: Running in strict mode without enabling the float_combinators pass can lead to some functions expanding infinitely."
      );
    }
    if !self.linearize_matches.enabled() {
      println!(
        "Warning: Running in strict mode without enabling the linearize_matches pass can lead to some functions expanding infinitely."
      );
    }
  }
}

impl Default for CompileOpts {
  /// Enables eta, linearize_matches, float_combinators.
  /// Uses num-scott ADT encoding.
  fn default() -> Self {
    Self {
      target_architecture: CompilerTarget::Unknown,
      eta: true,
      prune: false,
      linearize_matches: OptLevel::Enabled,
      float_combinators: true,
      merge: false,
      inline: false,
      check_net_size: true,
      type_check: true,
      adt_encoding: AdtEncoding::NumScott,
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub enum AdtEncoding {
  Scott,
  NumScott,
}

impl std::fmt::Display for AdtEncoding {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AdtEncoding::Scott => write!(f, "Scott"),
      AdtEncoding::NumScott => write!(f, "NumScott"),
    }
  }
}

pub struct CompileResult {
  pub diagnostics: Diagnostics,
  pub hvm_book: crate::hvm::ast::Book,
  pub labels: Labels,
}

fn maybe_grow<R, F>(f: F) -> R
where
  F: FnOnce() -> R,
{
  stacker::maybe_grow(1024 * 32, 1024 * 1024, f)
}
