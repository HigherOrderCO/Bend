#![feature(box_patterns)]
#![feature(let_chains)]

use diagnostics::{Info, Warning};
use hvmc::{
  ast::{self, Net},
  dispatch_dyn_net,
  host::Host,
  run::{DynNet, Heap, Rewrites},
  stdlib::LogDef,
};
use hvmc_net::{pre_reduce::pre_reduce_book, prune::prune_defs};
use net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::nets_to_hvmc};
use std::{
  str::FromStr,
  sync::{Arc, Mutex},
  time::Instant,
};
use term::{
  book_to_nets,
  display::{display_readback_errors, DisplayJoin},
  net_to_term::net_to_term,
  term_to_net::Labels,
  AdtEncoding, Book, Ctx, ReadbackError, Term,
};

pub mod diagnostics;
pub mod hvmc_net;
pub mod net;
pub mod term;

pub use term::load_book::load_file_to_book;

pub const ENTRY_POINT: &str = "main";
pub const HVM1_ENTRY_POINT: &str = "Main";

/// These are the names of builtin defs that are not in the hvm-lang book, but
/// are present in the hvm-core book. They are implemented using Rust code by
/// [`create_host`] and they can not be rewritten as hvm-lang functions.
pub const CORE_BUILTINS: [&str; 3] = ["HVM.log", "HVM.black_box", "HVM.print"];

/// Creates a host with the hvm-core primitive definitions built-in.
/// This needs the book as an Arc because the closure that logs
/// data needs access to the book.
pub fn create_host(book: Arc<Book>, labels: Arc<Labels>, compile_opts: CompileOpts) -> Arc<Mutex<Host>> {
  let host = Arc::new(Mutex::new(Host::default()));
  host.lock().unwrap().insert_def(
    "HVM.log",
    hvmc::host::DefRef::Owned(Box::new(LogDef::new({
      let host = host.clone();
      let book = book.clone();
      let labels = labels.clone();
      move |wire| {
        let host = host.lock().unwrap();
        let tree = host.readback_tree(&wire);
        let net = hvmc::ast::Net { root: tree, redexes: vec![] };
        let net = hvmc_to_net(&net);
        let (mut term, mut readback_errors) = net_to_term(&net, &book, &labels, false);
        let resugar_errs = term.resugar_adts(&book, compile_opts.adt_encoding);
        term.resugar_builtins();

        readback_errors.extend(resugar_errs);
        println!("{}{}", display_readback_errors(&readback_errors), term);
      }
    }))),
  );
  host.lock().unwrap().insert_def(
    "HVM.print",
    hvmc::host::DefRef::Owned(Box::new(LogDef::new({
      let host = host.clone();
      let book = book.clone();
      let labels = labels.clone();
      move |wire| {
        let host = host.lock().unwrap();
        let tree = host.readback_tree(&wire);
        let net = hvmc::ast::Net { root: tree, redexes: vec![] };
        let net = hvmc_to_net(&net);
        let (mut term, mut readback_errors) = net_to_term(&net, &book, &labels, false);
        let resugar_errs = term.resugar_adts(&book, compile_opts.adt_encoding);
        term.resugar_builtins();

        readback_errors.extend(resugar_errs);
        if let Term::Str { val } = term {
          println!("{val}");
        }
      }
    }))),
  );
  let book = ast::Book::from_str("@HVM.black_box = (x x)").unwrap();
  host.lock().unwrap().insert_book(&book);

  host
}

pub fn check_book(book: &mut Book) -> Result<(), Info> {
  // TODO: Do the checks without having to do full compilation
  // TODO: Shouldn't the check mode show warnings?
  compile_book(book, CompileOpts::light())?;
  Ok(())
}

pub fn compile_book(book: &mut Book, opts: CompileOpts) -> Result<CompileResult, Info> {
  let warns = desugar_book(book, opts)?;
  let (nets, labels) = book_to_nets(book);
  let mut core_book = nets_to_hvmc(nets)?;
  if opts.pre_reduce {
    pre_reduce_book(&mut core_book, book.hvmc_entrypoint())?;
  }
  if opts.prune {
    prune_defs(&mut core_book, book.hvmc_entrypoint().to_string());
  }
  Ok(CompileResult { core_book, labels, warns })
}

pub fn desugar_book(book: &mut Book, opts: CompileOpts) -> Result<Vec<Warning>, Info> {
  let mut ctx = Ctx::new(book);

  ctx.check_shared_names();
  ctx.set_entrypoint();

  ctx.book.encode_adts(opts.adt_encoding);
  ctx.book.encode_builtins();

  ctx.book.resolve_ctrs_in_pats();
  ctx.resolve_refs()?;

  ctx.check_match_arity()?;
  ctx.check_unbound_pats()?;

  ctx.book.desugar_let_destructors();
  ctx.book.desugar_implicit_match_binds();

  ctx.check_ctrs_arities()?;
  // Must be between [`Book::desugar_implicit_match_binds`] and [`Ctx::linearize_matches`]
  ctx.check_unbound_vars()?;

  ctx.book.convert_match_def_to_term();
  ctx.simplify_matches()?;

  if opts.linearize_matches.enabled() {
    ctx.linearize_simple_matches(opts.linearize_matches.is_extra())?;
  }

  ctx.book.encode_simple_matches(opts.adt_encoding);

  // sanity check
  ctx.check_unbound_vars()?;

  ctx.book.make_var_names_unique();
  ctx.book.linearize_vars();

  // sanity check
  ctx.check_unbound_vars()?;

  // Optimizing passes
  if opts.eta {
    ctx.book.eta_reduction();
  }
  if opts.float_combinators {
    ctx.book.float_combinators();
  }
  if opts.ref_to_ref {
    ctx.simplify_ref_to_ref()?;
  }
  if opts.simplify_main {
    ctx.book.simplify_main_ref();
  }

  ctx.prune(opts.prune, opts.adt_encoding);

  if opts.inline {
    ctx.book.inline();
  }
  if opts.merge {
    ctx.book.merge_definitions();
  }

  if !ctx.info.has_errors() { Ok(ctx.info.warns) } else { Err(ctx.info) }
}

pub fn run_book(
  mut book: Book,
  mem_size: usize,
  run_opts: RunOpts,
  warning_opts: WarningOpts,
  compile_opts: CompileOpts,
) -> Result<(Term, RunInfo), Info> {
  let CompileResult { core_book, labels, warns } = compile_book(&mut book, compile_opts)?;

  // Turn the book into an Arc so that we can use it for logging, debugging, etc.
  // from anywhere else in the program
  // This "freezes" the book and prevents further modification.
  let book = Arc::new(book);
  let labels = Arc::new(labels);

  display_warnings(&warns, warning_opts)?;

  // Run
  let debug_hook = run_opts.debug_hook(&book, &labels);
  let host = create_host(book.clone(), labels.clone(), compile_opts);
  host.lock().unwrap().insert_book(&core_book);

  let (res_lnet, stats) = run_compiled(host, mem_size, run_opts, debug_hook, book.hvmc_entrypoint());

  // Readback
  let net = hvmc_to_net(&res_lnet);
  let (mut res_term, mut readback_errors) = net_to_term(&net, &book, &labels, run_opts.linear);
  let resugar_errs = res_term.resugar_adts(&book, compile_opts.adt_encoding);
  res_term.resugar_builtins();

  readback_errors.extend(resugar_errs);
  let info = RunInfo { stats, readback_errors, net: res_lnet, book, labels };
  Ok((res_term, info))
}

/// Utility function to count the amount of nodes in an hvm-core AST net
pub fn count_nodes<'l>(net: &'l hvmc::ast::Net) -> usize {
  let mut visit: Vec<&'l hvmc::ast::Tree> = vec![&net.root];
  let mut count = 0usize;
  for (l, r) in &net.redexes {
    visit.push(l);
    visit.push(r);
  }
  while let Some(tree) = visit.pop() {
    match tree {
      ast::Tree::Ctr { lft, rgt, .. } | ast::Tree::Op2 { lft, rgt, .. } => {
        count += 1;
        visit.push(lft);
        visit.push(rgt);
      }
      ast::Tree::Op1 { rgt, .. } => {
        count += 1;
        visit.push(rgt);
      }
      ast::Tree::Mat { sel, ret } => {
        count += 1;
        visit.push(sel);
        visit.push(ret);
      }
      ast::Tree::Var { .. } => (),
      _ => {
        count += 1;
      }
    };
  }
  count
}

pub fn run_compiled(
  host: Arc<Mutex<Host>>,
  mem_size: usize,
  run_opts: RunOpts,
  hook: Option<impl FnMut(&Net)>,
  entrypoint: &str,
) -> (Net, RunStats) {
  let heap = Heap::new_bytes(mem_size);
  let mut root = DynNet::new(&heap, run_opts.lazy_mode);
  let max_rwts = run_opts.max_rewrites.map(|x| x.clamp(usize::MIN as u64, usize::MAX as u64) as usize);
  // Expect won't be reached because there's
  // a pass that checks this.
  dispatch_dyn_net!(&mut root => {
    root.boot(host.lock().unwrap().defs.get(entrypoint).expect("No main function."));

    let start_time = Instant::now();

    if let Some(mut hook) = hook {
      root.expand();
      while !root.redexes.is_empty() {
        hook(&host.lock().unwrap().readback(root));
        root.reduce(1);
        root.expand();
      }
    } else if let Some(mut max_rwts) = max_rwts {
      if run_opts.lazy_mode {
        panic!("Lazy mode does not yet support rewrite limit");
      }
      if !run_opts.single_core {
        panic!("Parallel mode does not yet support rewrite limit");
      }
      root.expand();
      while !root.redexes.is_empty() {
        let old_rwts = root.rwts.total();
        root.reduce(max_rwts);
        let delta_rwts = root.rwts.total() - old_rwts;
        if (max_rwts as u64) < delta_rwts {
          eprintln!("Warning: Exceeded max rwts");
          break;
        }
        max_rwts -= delta_rwts as usize;
        root.expand();
      }
    } else if !run_opts.single_core {
      root.parallel_normal();
    } else {
      root.normal();
    }
    let elapsed = start_time.elapsed().as_secs_f64();


    let net = host.lock().unwrap().readback(root);

    // TODO I don't quite understand this code
    // How would it be implemented in the new version?
    let stats = RunStats { rewrites: root.rwts, used: count_nodes(&net), run_time: elapsed };
    (net, stats)
  })
}

#[derive(Clone, Copy, Debug, Default)]
pub struct RunOpts {
  pub single_core: bool,
  pub debug: bool,
  pub linear: bool,
  pub lazy_mode: bool,
  pub max_memory: u64,
  pub max_rewrites: Option<u64>,
}

impl RunOpts {
  pub fn lazy() -> Self {
    Self { lazy_mode: true, single_core: true, ..Self::default() }
  }

  fn debug_hook<'a>(&'a self, book: &'a Book, labels: &'a Labels) -> Option<impl FnMut(&Net) + 'a> {
    self.debug.then_some({
      |net: &_| {
        let net = hvmc_to_net(net);
        let (res_term, errors) = net_to_term(&net, book, labels, self.linear);
        println!("{}{}\n---------------------------------------", display_readback_errors(&errors), res_term,)
      }
    })
  }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum OptLevel {
  #[default]
  Disabled,
  Enabled,
  Extra,
}

impl OptLevel {
  pub fn enabled(&self) -> bool {
    !matches!(self, OptLevel::Disabled)
  }

  pub fn is_extra(&self) -> bool {
    matches!(self, OptLevel::Extra)
  }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct CompileOpts {
  /// Selects the encoding for the ADT syntax.
  pub adt_encoding: AdtEncoding,

  /// Enables [term::transform::eta_reduction].
  pub eta: bool,

  /// Enables [term::transform::simplify_ref_to_ref].
  pub ref_to_ref: bool,

  /// Enables [term::transform::definition_pruning] and [hvmc_net::prune].
  pub prune: bool,

  /// Enables [hvmc_net::pre_reduce].
  pub pre_reduce: bool,

  /// Enables [term::transform::linearize_matches].
  pub linearize_matches: OptLevel,

  /// Enables [term::transform::float_combinators].
  pub float_combinators: bool,

  /// Enables [term::transform::simplify_main_ref].
  pub simplify_main: bool,

  /// Enables [term::transform::definition_merge]
  pub merge: bool,

  /// Enables [term::transform::inline].
  pub inline: bool,
}

impl CompileOpts {
  /// All optimizations enabled.
  pub fn heavy() -> Self {
    Self {
      eta: true,
      ref_to_ref: true,
      prune: true,
      pre_reduce: true,
      float_combinators: true,
      simplify_main: true,
      merge: true,
      inline: true,
      adt_encoding: Default::default(),
      linearize_matches: OptLevel::Extra,
    }
  }

  /// Set all opts as true and keep the current adt encoding.
  pub fn set_all(self) -> Self {
    Self { adt_encoding: self.adt_encoding, ..Self::heavy() }
  }

  /// Set all opts as false and keep the current adt encoding.
  pub fn set_no_all(self) -> Self {
    Self { adt_encoding: self.adt_encoding, ..Self::default() }
  }

  /// All optimizations disabled, except float_combinators and linearize_matches
  pub fn light() -> Self {
    Self { float_combinators: true, linearize_matches: OptLevel::Extra, ..Self::default() }
  }

  // Disable optimizations that don't work or are unnecessary on lazy mode
  pub fn lazy_mode(&mut self) {
    self.float_combinators = false;
    if self.linearize_matches.is_extra() {
      self.linearize_matches = OptLevel::Enabled;
    }
    self.pre_reduce = false;
  }
}

impl CompileOpts {
  pub fn check(&self, lazy_mode: bool) {
    if !lazy_mode {
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
}

#[derive(Default, Clone, Copy)]
pub struct WarningOpts {
  pub match_only_vars: WarnState,
  pub unused_defs: WarnState,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum WarnState {
  #[default]
  Warn,
  Allow,
  Deny,
}

impl WarningOpts {
  pub fn allow_all() -> Self {
    Self { match_only_vars: WarnState::Allow, unused_defs: WarnState::Allow }
  }

  pub fn deny_all() -> Self {
    Self { match_only_vars: WarnState::Deny, unused_defs: WarnState::Deny }
  }

  pub fn warn_all() -> Self {
    Self { match_only_vars: WarnState::Warn, unused_defs: WarnState::Warn }
  }

  /// Filters warnings based on the enabled flags.
  pub fn filter<'a>(&'a self, warns: &'a [Warning], ws: WarnState) -> Vec<&Warning> {
    warns
      .iter()
      .filter(|w| {
        (match w {
          Warning::MatchOnlyVars(_) => self.match_only_vars,
          Warning::UnusedDefinition(_) => self.unused_defs,
        }) == ws
      })
      .collect()
  }
}

/// Either just prints warnings or returns Err when any denied was produced.
pub fn display_warnings(warnings: &[Warning], warning_opts: WarningOpts) -> Result<(), String> {
  let warns = warning_opts.filter(warnings, WarnState::Warn);
  if !warns.is_empty() {
    eprintln!("Warnings:\n{}", DisplayJoin(|| warns.iter(), "\n"));
  }
  let denies = warning_opts.filter(warnings, WarnState::Deny);
  if !denies.is_empty() {
    return Err(format!(
      "{}\nCould not run the code because of the previous warnings",
      DisplayJoin(|| denies.iter(), "\n")
    ));
  }
  Ok(())
}

pub struct CompileResult {
  pub warns: Vec<Warning>,
  pub core_book: hvmc::ast::Book,
  pub labels: Labels,
}

impl std::fmt::Debug for CompileResult {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for warn in &self.warns {
      writeln!(f, "// WARNING: {}", warn)?;
    }
    write!(f, "{}", self.core_book)
  }
}

impl CompileResult {
  pub fn display_with_warns(&self, opts: WarningOpts) -> Result<String, String> {
    display_warnings(&self.warns, opts)?;
    Ok(self.core_book.to_string())
  }
}

pub struct RunInfo {
  pub stats: RunStats,
  pub readback_errors: Vec<ReadbackError>,
  pub net: Net,
  pub book: Arc<Book>,
  pub labels: Arc<Labels>,
}

pub struct RunStats {
  pub rewrites: Rewrites,
  pub used: usize,
  pub run_time: f64,
}
