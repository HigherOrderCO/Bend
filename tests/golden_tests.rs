use bend::{
  compile_book, desugar_book,
  diagnostics::{Diagnostics, DiagnosticsConfig, Severity},
  fun::{
    load_book::do_parse_core_book, net_to_term::net_to_term, parser::TermParser, term_to_net::Labels, Book,
    Ctx, Name,
  },
  net::hvmc_to_net::hvmc_to_net,
  run_book, CompileOpts, RunOpts,
};
use insta::assert_snapshot;
use itertools::Itertools;
use std::{
  collections::HashMap,
  fmt::Write,
  io::Read,
  path::{Path, PathBuf},
  str::FromStr,
};
use stdext::function_name;
use walkdir::WalkDir;

// Since running a program requires messing with stdout and stderr,
// if we run multiple at the same time, their outputs can get mixed.
// So we put a mutex to execute only one "run" test at a time.
static RUN_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());

const TESTS_PATH: &str = "/tests/golden_tests/";

type RunFn = dyn Fn(&str, &Path) -> Result<String, Diagnostics>;

fn run_single_golden_test(path: &Path, run: &[&RunFn]) -> Result<(), String> {
  println!("{}", path.display());
  let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
  let file_name = path.to_str().and_then(|path| path.rsplit_once(TESTS_PATH)).unwrap().1;

  // unfortunately we need to do this
  let file_path = format!("{}{}", &TESTS_PATH[1 ..], file_name);
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

  let walker = WalkDir::new(&root).sort_by_file_name().max_depth(2).into_iter().filter_entry(|e| {
    let path = e.path();
    path == root || path.is_dir() || (path.is_file() && path.extension().is_some_and(|x| x == "hvm"))
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

#[test]
fn compile_term() {
  run_golden_test_dir(function_name!(), &|code, _| {
    let mut term = TermParser::new(code).parse_term()?;
    let mut vec = Vec::new();
    term.check_unbound_vars(&mut HashMap::new(), &mut vec);

    if !vec.is_empty() {
      return Err(vec.into_iter().map(|e| e.to_string()).join("\n").into());
    }

    term.make_var_names_unique();
    term.linearize_vars();
    let net = bend::fun::term_to_net(&term, &mut Default::default()).map_err(|e| e.to_string())?;

    Ok(format!("{}", net))
  })
}

#[test]
fn compile_core() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig { unused_definition: Severity::Allow, ..Default::default() };

    let res = compile_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, res.core_book))
  })
}

#[test]
fn compile_core_o_all() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig {
      recursion_cycle: Severity::Warning,
      unused_definition: Severity::Allow,
      ..Default::default()
    };

    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, res.core_book))
  })
}

#[test]
fn compile_core_o_no_all() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let compile_opts = CompileOpts::default().set_no_all();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let res = compile_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(format!("{}", res.core_book))
  })
}

#[test]
fn linear_readback() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = do_parse_core_book(code, path, Book::builtins())?;
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let (term, _, diags) = run_book(
      book,
      RunOpts { linear_readback: true, ..Default::default() },
      compile_opts,
      diagnostics_cfg,
      None,
    )?;
    let res = format!("{diags}{term}");
    Ok(res)
  });
}

#[test]
fn run_core() {
  run_golden_test_dir_multiple(function_name!(), &[(&|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = do_parse_core_book(code, path, Book::builtins())?;
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig {
      unused_definition: Severity::Allow,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let run_opts = RunOpts::default();

    let (term, _, diags) = run_book(book, run_opts, compile_opts, diagnostics_cfg, None)?;
    let res = format!("{diags}{term}");
    Ok(res)
  })])
}

#[test]
#[ignore = "while lazy execution is not implemented for hvm32"]
fn run_lazy() {
  run_golden_test_dir(function_name!(), &|_code, _path| {
    todo!()
    /* let _guard = RUN_MUTEX.lock().unwrap();
    let book = do_parse_book(code, path)?;
    let compile_opts = CompileOpts::default_lazy();
    let diagnostics_cfg = DiagnosticsConfig {
      recursion_cycle: Severity::Allow,
      unused_definition: Severity::Allow,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let run_opts = RunOpts::lazy();

    let (term, _, diags) = run_book(book, run_opts, compile_opts, diagnostics_cfg, None)?;
    let res = format!("{diags}{term}");
    Ok(res) */
  })
}

#[test]
fn readback_lnet() {
  run_golden_test_dir(function_name!(), &|code, _| {
    let net = hvmc::ast::Net::from_str(code)?;
    let book = Book::default();
    let compat_net = hvmc_to_net(&net);
    let mut diags = Diagnostics::default();
    let term = net_to_term(&compat_net, &book, &Labels::default(), false, &mut diags);
    Ok(format!("{}{}", diags, term))
  })
}

#[test]
fn simplify_matches() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let diagnostics_cfg = DiagnosticsConfig::new(Severity::Error, true);
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let mut ctx = Ctx::new(&mut book, diagnostics_cfg);

    ctx.check_shared_names();
    ctx.set_entrypoint();
    ctx.book.encode_adts();
    ctx.fix_match_defs()?;
    ctx.book.encode_builtins();
    ctx.resolve_refs()?;
    ctx.desugar_match_defs()?;
    ctx.fix_match_terms()?;
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.linearize_match_binds();
    ctx.book.linearize_match_with();
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.apply_use();
    ctx.book.make_var_names_unique();
    ctx.prune(false);

    Ok(ctx.book.to_string())
  })
}

#[test]
fn parse_core() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let book = do_parse_core_book(code, path, Book::builtins())?;
    Ok(book.to_string())
  })
}

#[test]
fn encode_pattern_match() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut result = String::new();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let mut ctx = Ctx::new(&mut book, diagnostics_cfg);
    ctx.check_shared_names();
    ctx.set_entrypoint();
    ctx.book.encode_adts();
    ctx.fix_match_defs()?;
    ctx.book.encode_builtins();
    ctx.resolve_refs()?;
    ctx.desugar_match_defs()?;
    ctx.fix_match_terms()?;
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.linearize_match_binds();
    ctx.book.linearize_match_with();
    ctx.book.encode_matches();
    ctx.check_unbound_vars()?;
    ctx.book.make_var_names_unique();
    ctx.book.apply_use();
    ctx.book.make_var_names_unique();
    ctx.book.linearize_vars();
    ctx.prune(false);

    writeln!(result, "{}\n", ctx.book).unwrap();
    Ok(result)
  })
}

#[test]
fn desugar_core() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig {
      unused_definition: Severity::Allow,
      ..DiagnosticsConfig::new(Severity::Error, true)
    };
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    desugar_book(&mut book, compile_opts, diagnostics_cfg, None)?;
    Ok(book.to_string())
  })
}

#[test]
#[ignore = "bug - the subprocess created by run_book leaks"]
fn hangs() {
  let expected_normalization_time = 5;

  run_golden_test_dir(function_name!(), &move |code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let book = do_parse_core_book(code, path, Book::builtins())?;
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig::new(Severity::Allow, false);

    let thread =
      std::thread::spawn(move || run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None));
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

#[test]
fn compile_entrypoint() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    book.entrypoint = Some(Name::new("foo"));
    let diagnostics_cfg = DiagnosticsConfig { ..DiagnosticsConfig::new(Severity::Error, true) };
    let res = compile_book(&mut book, CompileOpts::default(), diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, res.core_book))
  })
}

#[test]
#[ignore = "while execution with different entrypoints is not implemented for hvm32"]
fn run_entrypoint() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let _guard = RUN_MUTEX.lock().unwrap();
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    book.entrypoint = Some(Name::new("foo"));
    let compile_opts = CompileOpts::default().set_all();
    let diagnostics_cfg = DiagnosticsConfig { ..DiagnosticsConfig::new(Severity::Error, true) };
    let (term, _, diags) = run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None)?;
    let res = format!("{diags}{term}");
    Ok(res)
  })
}

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

#[test]
fn mutual_recursion() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let diagnostics_cfg =
      DiagnosticsConfig { recursion_cycle: Severity::Error, ..DiagnosticsConfig::new(Severity::Allow, true) };
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let opts = CompileOpts { merge: true, ..CompileOpts::default() };
    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, res.core_book))
  })
}

#[test]
#[ignore = "while IO is not implemented for hvm32"]
fn io() {
  run_golden_test_dir_multiple(function_name!(), &[
    /* (&|code, path| {
      let _guard = RUN_MUTEX.lock().unwrap();
      let book = do_parse_book(code, path)?;
      let compile_opts = CompileOpts::default_lazy();
      let diagnostics_cfg = DiagnosticsConfig::default_lazy();
      let Output { status, stdout, stderr } =
        run_book(book, None, RunOpts::lazy(), compile_opts, diagnostics_cfg, None)?;
      let stderr = String::from_utf8_lossy(&stderr);
      let status = if !status.success() { format!("\n{status}") } else { String::new() };
      let stdout = String::from_utf8_lossy(&stdout);
      Ok(format!("Lazy mode:\n{}{}{}", stderr, status, stdout))
    }), */
    (&|code, path| {
      let _guard = RUN_MUTEX.lock().unwrap();
      let book = do_parse_core_book(code, path, Book::builtins())?;
      let compile_opts = CompileOpts::default();
      let diagnostics_cfg = DiagnosticsConfig::default();
      let (term, _, diags) = run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None)?;
      let res = format!("{diags}{term}");
      Ok(format!("Strict mode:\n{res}"))
    }),
  ])
}

#[test]
fn examples() -> Result<(), Diagnostics> {
  let examples_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples");

  for entry in WalkDir::new(examples_path)
    .min_depth(1)
    .into_iter()
    .filter_map(|e| e.ok())
    .filter(|e| e.path().extension().map_or(false, |ext| ext == "hvm"))
  {
    let _guard = RUN_MUTEX.lock().unwrap();
    let path = entry.path();
    eprintln!("Testing {}", path.display());
    let code = std::fs::read_to_string(path).map_err(|e| e.to_string())?;

    let book = do_parse_core_book(&code, path, Book::builtins()).unwrap();
    let compile_opts = CompileOpts::default();
    let diagnostics_cfg = DiagnosticsConfig::default();
    let (term, _, diags) = run_book(book, RunOpts::default(), compile_opts, diagnostics_cfg, None)?;
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

#[test]
fn scott_triggers_unused() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_core_book(code, path, Book::builtins())?;
    let opts = CompileOpts::default();
    let diagnostics_cfg =
      DiagnosticsConfig { unused_definition: Severity::Error, ..DiagnosticsConfig::default() };
    let res = compile_book(&mut book, opts, diagnostics_cfg, None)?;
    Ok(format!("{}{}", res.diagnostics, res.core_book))
  })
}
