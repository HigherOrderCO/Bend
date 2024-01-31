use hvmc::ast::{parse_net, show_net};
use hvml::{
  compile_book, encode_pattern_matching,
  net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::net_to_hvmc},
  run_book,
  term::{
    load_book::do_parse_book, net_to_term::net_to_term, parser::parse_term, term_to_compat_net, Book, DefId,
    Term,
  },
  Opts, WarningOpts,
};
use insta::assert_snapshot;
use itertools::Itertools;
use std::{
  collections::HashMap,
  fs,
  path::{Path, PathBuf},
  sync::{Arc, RwLock},
};
use stdext::function_name;
use walkdir::WalkDir;

fn do_parse_term(code: &str) -> Result<Term, String> {
  parse_term(code).map_err(|errs| errs.into_iter().map(|e| e.to_string()).join("\n"))
}

fn do_parse_net(code: &str) -> Result<hvmc::ast::Net, String> {
  parse_net(&mut code.chars().peekable())
}

const TESTS_PATH: &str = "/tests/golden_tests/";

fn run_single_golden_test(
  path: &Path,
  run: &dyn Fn(&str, &Path) -> Result<String, String>,
) -> Result<(), String> {
  let code = fs::read_to_string(path).map_err(|e| e.to_string())?;
  let file_name = path.to_str().and_then(|path| path.rsplit_once(TESTS_PATH)).unwrap().1;

  // unfortunately we need to do this
  let file_path = format!("{}{}", &TESTS_PATH[1 ..], file_name);
  let file_path = Path::new(&file_path);

  let result: String = run(&code, file_path).unwrap_or_else(|err| err);

  let mut settings = insta::Settings::clone_current();
  settings.set_prepend_module_to_snapshot(false);
  settings.set_omit_expression(true);
  settings.set_input_file(path);

  settings.bind(|| {
    assert_snapshot!(file_name, result);
  });

  Ok(())
}

fn run_golden_test_dir(test_name: &str, run: &dyn Fn(&str, &Path) -> Result<String, String>) {
  let root = PathBuf::from(format!(
    "{}{TESTS_PATH}{}",
    env!("CARGO_MANIFEST_DIR"),
    test_name.rsplit_once(':').unwrap().1
  ));

  let walker = WalkDir::new(&root).sort_by_file_name().max_depth(2).into_iter().filter_entry(|e| {
    let path = e.path();
    path == root || (path.is_file() && path.extension().is_some_and(|x| x == "hvm"))
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

#[test]
fn compile_term() {
  run_golden_test_dir(function_name!(), &|code, _| {
    let mut term = do_parse_term(code)?;
    term.check_unbound_vars(&mut HashMap::new())?;
    term.make_var_names_unique();
    term.linearize_vars();
    let compat_net = term_to_compat_net(&term, &mut Default::default());
    let net = net_to_hvmc(&compat_net, &|def_id| def_id.to_internal())?;

    let result = show_net(&net);

    Ok(result)
  })
}

#[test]
fn compile_file_o_all() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_book(code, path)?;
    let compiled = compile_book(&mut book, Opts::heavy())?;
    Ok(format!("{:?}", compiled))
  })
}
#[test]
fn compile_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_book(code, path)?;
    let compiled = compile_book(&mut book, Opts::light())?;
    Ok(format!("{:?}", compiled))
  })
}

#[test]
fn run_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let book = do_parse_book(code, path)?;
    // 1 million nodes for the test runtime. Smaller doesn't seem to make it any faster
    let (res, def_names, info) =
      run_book(book, 1 << 20, true, false, false, WarningOpts::deny_all(), Opts::heavy())?;
    Ok(format!("{}{}", info.readback_errors.display(&def_names), res.display(&def_names)))
  })
}

#[test]
fn readback_lnet() {
  run_golden_test_dir(function_name!(), &|code, _| {
    let net = do_parse_net(code)?;
    let book = Book::default();
    let compat_net = hvmc_to_net(&net, &DefId::from_internal);
    let (term, errors) = net_to_term(&compat_net, &book, &Default::default(), false);
    Ok(format!("{}{}", errors.display(&book.def_names), term.display(&book.def_names)))
  })
}

#[test]
fn flatten_rules() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_book(code, path)?;
    let main = book.check_has_main().ok();
    book.check_shared_names()?;
    book.encode_builtins();
    book.resolve_ctrs_in_pats();
    book.generate_scott_adts();
    book.desugar_let_destructors();
    book.desugar_implicit_match_binds();
    book.check_unbound_pats()?;
    book.extract_adt_matches(&mut Vec::new())?;
    book.flatten_rules();
    book.prune(main, false, &mut Vec::new());
    Ok(book.to_string())
  })
}

#[test]
fn parse_file() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let book = do_parse_book(code, path)?;
    Ok(book.to_string())
  })
}

#[test]
fn encode_pattern_match() {
  run_golden_test_dir(function_name!(), &|code, path| {
    let mut book = do_parse_book(code, path)?;
    let main = book.check_has_main().ok();
    book.check_shared_names()?;
    book.encode_builtins();
    book.generate_scott_adts();
    book.resolve_refs()?;
    encode_pattern_matching(&mut book, &mut Vec::new())?;
    book.prune(main, false, &mut Vec::new());
    Ok(book.to_string())
  })
}

#[test]
#[ignore = "To not delay golden tests execution"]
fn hangs() {
  let expected_normalization_time = 1;

  run_golden_test_dir(function_name!(), &|code, path| {
    let book = do_parse_book(code, path)?;

    let lck = Arc::new(RwLock::new(false));
    let got = lck.clone();
    std::thread::spawn(move || {
      let _ = run_book(book, 1 << 20, true, false, false, WarningOpts::deny_all(), Opts::heavy());
      *got.write().unwrap() = true;
    });
    std::thread::sleep(std::time::Duration::from_secs(expected_normalization_time));

    if !*lck.read().unwrap() { Ok("Hangs".into()) } else { Err("Doesn't hang".into()) }
  })
}
