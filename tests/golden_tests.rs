//! This module runs snapshot tests for compiling and running Bend programs.
//!
//! The result of each test is saved as a snapshot and used as golden output
//! for future tests. This allows us to test regressions in compilation and
//! have a history of how certain programs compiled and ran.
//!
//! These tests use `cargo-insta`. To run the tests, run `cargo insta test`.
//! If there are any changes to the snapshots, they'll be highlighted by the
//! CLI tool. Then, run `cargo insta review` to review these changes.

use bend::{
  check_book, compile_book, desugar_book,
  diagnostics::{Diagnostics, DiagnosticsConfig, Severity},
  fun::{
    load_book::do_parse_book, net_to_term::net_to_term, parser::ParseBook, term_to_net::Labels, Book, Ctx,
    Name,
  },
  hvm::hvm_book_show_pretty,
  imports::DefaultLoader,
  load_to_book,
  net::hvm_to_net::hvm_to_net,
  run_book, AdtEncoding, CompileOpts, RunOpts,
};
use insta::assert_snapshot;
use itertools::Itertools;
use std::{
  collections::HashMap,
  fmt::Write,
  io::Read,
  path::{Path, PathBuf},
};
use stdext::function_name;
use walkdir::WalkDir;

// Since running a program requires messing with stdout and stderr,
// if we run multiple at the same time, their outputs can get mixed.
// So we put a mutex to execute only one "run" test at a time.
static RUN_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());

const TESTS_PATH: &str = "/tests/golden_tests/";

type RunFn = dyn Fn(&str, &Path) -> Result<String, Diagnostics>;

pub fn parse_book_single_file(code: &str, origin: &Path) -> Result<Book, Diagnostics> {
  do_parse_book(code, origin, ParseBook::builtins())?.to_fun()
}

fn run_single_golden_test(path: &Path, run: &[&RunFn]) -> Result<(), String> {
  println!("{}", path.display());
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  let file_name = path.to_str().and_then(|path| path.rsplit_once(TESTS_PATH)).unwrap().1;

  // unfortunately we need to do this
  let file_path = format!("{}{}", &TESTS_PATH[1..], file_name);
  let file_path = Path::new(&file_path);

  let mut results: HashMap<&Path, Vec<String>> = HashMap::new();
  for fun in run {
    let result = fun(&code, file_path).unwrap_or_else(|err| err.to_string());
    results.entry(file_path).or_default().push(result);
  }
  let results = results.into_values().map(|v| v.join("\n")).collect_vec();

  let mut settings = insta::Settings::clone_current();
  settings.set_prepend_module_to_snapshot(false);
  settings.set_omit_expression(true);
  settings.set_input_file(path);

  settings.bind(|| {
    for result in results {
      assert_snapshot!(file_name, result);
    }
  });

  Ok(())
}

fn run_golden_test_dir(test_name: &str, run: &RunFn) {
  run_golden_test_dir_multiple(test_name, &[run])
}

fn run_golden_test_dir_multiple(test_name: &str, run: &[&RunFn]) {
  let root = PathBuf::from(format!(
    "{}{TESTS_PATH}{}",
    env!("CARGO_MANIFEST_DIR"),
    test_name.rsplit_once(':').unwrap().1
  ));

  let walker = WalkDir::new(&root).sort_by_file_name().max_depth(1).into_iter().filter_entry(|e| {
    let path = e.path();
    path == root || path.is_dir() || (path.is_file() && path.extension().is_some_and(|x| x == "bend"))
  });

  for entry in walker {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.is_file() {
      eprintln!("Testing {}", path.display());
      run_single_golden_test(path, run).unwrap();
    }
  }
}

/* Snapshot/regression/golden tests

 Each tests runs all the files in tests/golden_tests/<test name>.

 The test functions decide how exactly to process the test programs
 and what to save as a snapshot.
*/

/// Compiles a file with regular compilation options.
#[test]
fn compile_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig { unused_definition: Severity::Allow, ..Default::default() };

    let res = compile_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, hvm_book_show_pretty(&res.hvm_book)))
  })
}

/// Compiles a file with `-Oall` option.
#[test]
fn compile_file_o_all() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig {
      recursion_cycle: Severity::Warning,
      unused_definition: Severity::Allow,
      ..Default::default()
    };

    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, hvm_book_show_pretty(&res.hvm_book)))
  })
}

/// Compiles a file with `-Ono-all` option.
#[test]
fn compile_file_o_no_all() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let compile_opts = CompileOpts::default().set_no_all();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let res = compile_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(hvm_book_show_pretty(&res.hvm_book).to_string())
  })
}

/// Runs a file, but with linear readback enabled.
#[test]
fn linear_readback() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = parse_book_single_file(code, path)?;
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let (term, _, diags) = run_book(
      book,
      RunOpts { linear_readback: true, ..Default::default() },
      compile_opts,
      diagnostics_cfg,
      None,
      "run",
    )?
    .unwrap();
    let res = format!("{diags}{term}");
    Ok(res)
  });
}

/// Runs a file with regular compilation options, but rejecting all warnings.
/// Runs once for each ADT encoding.
#[test]
fn run_file() {
  run_golden_test_dir_multiple(
    function_name!(),
    &[(&|code, path| {
      let _guard = RUN_MUTEX.lock().unwrap();
      let book = parse_book_single_file(code, path)?;
      let diagnostics_cfg = DiagnosticsConfig {
        unused_definition: Severity::Allow,
        ..DiagnosticsConfig::new(Severity::Error, true)
      };
      let run_opts = RunOpts::default();

      let mut res = String::new();

      for adt_encoding in [AdtEncoding::NumScott, AdtEncoding::Scott] {
        let compile_opts = CompileOpts { adt_encoding, ..CompileOpts::default() };
        let (term, _, diags) =
          run_book(book.clone(), run_opts.clone(), compile_opts, diagnostics_cfg, None, "run")?.unwrap();
        res.push_str(&format!("{adt_encoding}:\n{diags}{term}\n\n"));
      }
      Ok(res)
    })],
  )
}

/// Runs bend programs, all sharing a common lib to test the import system.
#[test]
fn import_system() {
  run_golden_test_dir_multiple(
    function_name!(),
    &[(&|code, path| {
      let _guard = RUN_MUTEX.lock().unwrap();
      let diagnostics_cfg = DiagnosticsConfig {
        unused_definition: Severity::Allow,
        ..DiagnosticsConfig::new(Severity::Error, true)
      };

      let book = load_to_book(path, code, DefaultLoader::new(path), diagnostics_cfg)?;
      let run_opts = RunOpts::default();

      let mut res = String::new();

      let compile_opts = CompileOpts::default();
      let (term, _, diags) = run_book(book, run_opts, compile_opts, diagnostics_cfg, None, "run")?.unwrap();
      res.push_str(&format!("{diags}{term}\n\n"));
      Ok(res)
    })],
  )
}

/// Reads back an HVM net.
#[test]
fn readback_hvm() {
  run_golden_test_dir(function_name!(), &|code, _| {
    let mut p = hvm::ast::CoreParser::new(code);
    let net = p.parse_net()?;
    let book = Book::default();
    let compat_net = hvm_to_net(&net);
    let mut diags = Diagnostics::default();
    let term = net_to_term(&compat_net, &book, &Labels::default(), false, &mut diags);
    Ok(format!("{}{}", diags, term))
  })
}

/// Runs compilation up to fixing, simplifying and linearizing matches.
#[test]
fn simplify_matches() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let diagnostics_cfg = DiagnosticsConfig {
      unused_definition: Severity::Allow,
      irrefutable_match: Severity::Warning,
      unreachable_match: Severity::Warning,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let mut book = parse_book_single_file(code, path)?;
    let mut ctx = Ctx::new(&mut book, diagnostics_cfg);

    ctx.check_shared_names();
    ctx.book.encode_adts(AdtEncoding::NumScott);
    ctx.fix_match_defs()?;
    ctx.desugar_open()?;
    ctx.book.encode_builtins();
    ctx.resolve_refs()?;
    ctx.resolve_type_ctrs()?;
    ctx.desugar_match_defs()?;
    ctx.fix_match_terms()?;
    ctx.book.lift_local_defs();
    ctx.desugar_bend()?;
    ctx.desugar_fold()?;
    ctx.desugar_with_blocks()?;
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.desugar_use();
    ctx.book.linearize_match_binds();
    ctx.book.linearize_match_with();
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.desugar_use();
    ctx.book.make_var_names_unique();
    ctx.prune(false);

    Ok(format!("{}\n{}", ctx.book, ctx.info))
  })
}

/// Runs compilation up to encoding `match` terms as lambdas.
#[test]
fn encode_pattern_match() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut result = String::new();
    for adt_encoding in [AdtEncoding::Scott, AdtEncoding::NumScott] {
      let diagnostics_cfg = DiagnosticsConfig::default();
      let mut book = parse_book_single_file(code, path)?;
      let mut ctx = Ctx::new(&mut book, diagnostics_cfg);
      ctx.check_shared_names();
      ctx.book.encode_adts(adt_encoding);
      ctx.fix_match_defs()?;
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
      ctx.book.make_var_names_unique();
      ctx.book.desugar_use();
      ctx.book.linearize_match_binds();
      ctx.book.linearize_match_with();
      ctx.book.encode_matches(adt_encoding);
      ctx.check_unbound_vars()?;
      ctx.book.make_var_names_unique();
      ctx.book.desugar_use();
      ctx.book.make_var_names_unique();
      ctx.book.linearize_vars();
      ctx.prune(false);

      writeln!(result, "{adt_encoding}\n{}\n", ctx.book).unwrap();
    }
    Ok(result)
  })
}

/// Parses a file, but does not desugar or compile it.
#[test]
fn parse_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let mut ctx = Ctx::new(&mut book, Default::default());
    ctx.set_entrypoint();
    ctx.book.encode_adts(AdtEncoding::NumScott);
    ctx.book.encode_builtins();
    ctx.resolve_refs().expect("Resolve refs");
    ctx.prune(false);
    Ok(book.to_string())
  })
}

/// Runs the check command on a file.
#[test]
fn check_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig {
      unused_definition: Severity::Allow,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let mut book = parse_book_single_file(code, path)?;
    check_book(&mut book, diagnostics_cfg, compile_opts)?;
    Ok(book.to_string())
  })
}

/// Runs compilation up to the last term-level pass (`bend desugar` command).
#[test]
fn desugar_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig {
      unused_definition: Severity::Allow,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let mut book = parse_book_single_file(code, path)?;
    desugar_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(book.to_string())
  })
}

/// Runs a file that is expected to hang.
#[test]
#[ignore = "bug - the subprocess created by run_book leaks"]
fn hangs() {
  let expected_normalization_time = 5;

  run_golden_test_dir(function_name!(), &move |code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = parse_book_single_file(code, path)?;
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig::new(Severity::Allow, false);

    let thread = std::thread::spawn(move || {
      run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None, "run")
    });
    std::thread::sleep(std::time::Duration::from_secs(expected_normalization_time));

    if !thread.is_finished() {
      Ok("Hangs".into())
    } else if let Err(diags) = thread.join().unwrap() {
      Err(format!("Doesn't hang. (Compilation failed)\n{diags}").into())
    } else {
      Err("Doesn't hang. (Ran to the end)".to_string().into())
    }
  })
}

/// Compiles a file with a custom entrypoint.
#[test]
fn compile_entrypoint() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    book.entrypoint = Some(Name::new("foo"));
    let diagnostics_cfg = DiagnosticsConfig { ..DiagnosticsConfig::new(Severity::Error, true) };
    let res = compile_book(&mut book, CompileOpts::default(), diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, hvm_book_show_pretty(&res.hvm_book)))
  })
}

/// Runs a file with a custom entrypoint.
#[test]
#[ignore = "while execution with different entrypoints is not implemented for hvm32"]
fn run_entrypoint() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let mut book = parse_book_single_file(code, path)?;
    book.entrypoint = Some(Name::new("foo"));
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig { ..DiagnosticsConfig::new(Severity::Error, true) };
    let (term, _, diags) =
      run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None, "run")?.unwrap();
    let res = format!("{diags}{term}");
    Ok(res)
  })
}

/// Runs a Bend CLI command.
#[test]
fn cli() {
  run_golden_test_dir(function_name!(), &|_code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let mut args_path = PathBuf::from(path);
    assert!(args_path.set_extension("args"));

    let mut args_buf = String::with_capacity(16);
    let mut args_file = std::fs::File::open(args_path).expect("File exists");
    args_file.read_to_string(&mut args_buf).expect("Read args");
    let args = args_buf.lines();

    let output =
      std::process::Command::new(env!("CARGO_BIN_EXE_bend")).args(args).output().expect("Run command");
    let res =
      format!("{}{}", String::from_utf8_lossy(&output.stderr), String::from_utf8_lossy(&output.stdout));
    Ok(res)
  })
}

/// Compiles a file to check for mutual recursion.
#[test]
fn mutual_recursion() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let diagnostics_cfg =
      DiagnosticsConfig { recursion_cycle: Severity::Error, ..DiagnosticsConfig::new(Severity::Allow, true) };
    let mut book = parse_book_single_file(code, path)?;
    let opts = CompileOpts { merge: true, ..CompileOpts::default() };
    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, hvm_book_show_pretty(&res.hvm_book)))
  })
}

/// Runs a file that uses IO.
#[test]
fn io() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = parse_book_single_file(code, path)?;
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let (term, _, diags) =
      run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None, "run-c")?.unwrap();
    let res = format!("{diags}{term}");
    Ok(format!("Strict mode:\n{res}"))
  })
}

/// Runs all examples in the examples folder.
#[test]
fn examples() -> Result<(), Diagnostics> {
  let examples_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");

  for entry in WalkDir::new(examples_path)
    .min_depth(1)
    .into_iter()
    .filter_map(|e| e.ok())
    .filter(|e| e.path().extension().map_or(false, |ext| ext == "bend"))
  {
    let _guard = RUN_MUTEX.lock().unwrap_or_else(|e| e.into_inner());
    let path = entry.path();
    eprintln!("Testing {}", path.display());
    let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;

    let book = parse_book_single_file(&code, path).unwrap();
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let (term, _, diags) =
      run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None, "run-c")?.unwrap();
    let res = format!("{diags}{term}");

    let mut settings = insta::Settings::clone_current();
    settings.set_prepend_module_to_snapshot(false);
    settings.set_omit_expression(true);
    settings.set_input_file(path);

    settings.bind(|| {
      assert_snapshot!(format!("examples__{}", path.file_name().unwrap().to_str().unwrap()), res);
    });
  }

  Ok(())
}

/// Test that the Scott encoding correctly triggers unused definition warnings.
#[test]
fn scott_triggers_unused() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let opts = CompileOpts::default();
    let diagnostics_cfg =
      DiagnosticsConfig { unused_definition: Severity::Error, ..DiagnosticsConfig::default() };
    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, hvm_book_show_pretty(&res.hvm_book)))
  })
}

// TODO: also run the long string file to test the readback
/// Compiles a file that is very large and takes a long time to compile.
/// Only outputs if compilation worked without errors.
#[test]
fn compile_long() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = parse_book_single_file(code, path)?;
    let opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig {
      recursion_cycle: Severity::Warning,
      unused_definition: Severity::Allow,
      ..Default::default()
    };

    compile_book(&mut book, opts.clone(), diagnostics_cfg, None)?;
    Ok("Compiled".to_string())
  })
}
