use hvmc::ast::{parse_net, show_net};
use hvml::{
  compile_book,
  net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::net_to_hvmc},
  run_book,
  term::{
    net_to_term::net_to_term_non_linear,
    parser::{parse_definition_book, parse_term},
    term_to_compat_net, Book, DefId, Term, term_to_net::LabelGenerator,
  },
  Warning,
};
use insta::assert_snapshot;
use itertools::Itertools;
use std::{
  fs,
  path::{Path, PathBuf},
};
use stdext::function_name;
use walkdir::WalkDir;

fn do_parse_book(code: &str) -> Result<Book, String> {
  match parse_definition_book(code) {
    Ok(book) => Ok(book),
    Err(errs) => Err(errs.into_iter().map(|e| e.to_string()).join("\n")),
  }
}

fn do_parse_term(code: &str) -> Result<Term, String> {
  parse_term(code).map_err(|errs| errs.into_iter().map(|e| e.to_string()).join("\n"))
}

fn do_parse_net(code: &str) -> Result<hvmc::ast::Net, String> {
  parse_net(&mut code.chars().peekable())
}

const TESTS_PATH: &str = "/tests/golden_tests/";

fn run_single_golden_test(path: &Path, run: &dyn Fn(&str) -> Result<String, String>) -> Result<(), String> {
  let code = fs::read_to_string(path).map_err(|e| e.to_string())?;
  let file_name = path.to_str().and_then(|path| path.rsplit_once(TESTS_PATH)).unwrap().1;

  let result: String = run(&code).unwrap_or_else(|err| err);

  let mut settings = insta::Settings::clone_current();
  settings.set_prepend_module_to_snapshot(false);
  settings.set_omit_expression(true);
  settings.set_input_file(path);

  settings.bind(|| {
    assert_snapshot!(file_name, result);
  });

  Ok(())
}

fn run_golden_test_dir(test_name: &str, run: &dyn Fn(&str) -> Result<String, String>) {
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
      run_single_golden_test(path, run).unwrap();
    }
  }
}

#[test]
fn compile_term() {
  run_golden_test_dir(function_name!(), &|code| {
    let mut term = do_parse_term(code)?;
    term.check_unbound_vars()?;
    term.make_var_names_unique();
    term.linearize_vars();
    let mut label_generator = LabelGenerator::default();
    let (compat_net, dups) = term_to_compat_net(&term, &mut label_generator);
    let net = net_to_hvmc(&compat_net, &|def_id| def_id.to_internal())?;

    let result = if dups > hvml::net::MAX_DUP_HVMC_LABEL {
      format!("// {}\n{}", Warning::TooManyDups { name: String::new() }, show_net(&net))
    } else {
      show_net(&net)
    };

    Ok(result)
  })
}

#[test]
fn compile_file() {
  run_golden_test_dir(function_name!(), &|code| {
    let mut book = do_parse_book(code)?;
    let compiled = compile_book(&mut book)?;
    Ok(format!("{:?}", compiled))
  })
}

#[test]
fn run_single_files() {
  run_golden_test_dir(function_name!(), &|code| {
    let book = do_parse_book(code)?;
    // 1 million nodes for the test runtime. Smaller doesn't seem to make it any faster
    let (res, def_names, info) = run_book(book, 1 << 20, true, false)?;
    let res = if info.valid_readback {
      res.to_string(&def_names)
    } else {
      format!("Invalid readback\n{}", res.to_string(&def_names))
    };
    Ok(res)
  })
}

#[test]
fn readback_lnet() {
  run_golden_test_dir(function_name!(), &|code| {
    let net = do_parse_net(code)?;
    let book = Book::default();
    let compat_net = hvmc_to_net(&net, &DefId::from_internal);
    let (term, valid) = net_to_term_non_linear(&compat_net, &book);
    if valid {
      Ok(term.to_string(&book.def_names))
    } else {
      Ok(format!("Invalid readback:\n{}", term.to_string(&book.def_names)))
    }
  })
}

#[test]
fn flatten_rules() {
  run_golden_test_dir(function_name!(), &|code| {
    let mut book = do_parse_book(code)?;
    book.flatten_rules();
    Ok(book.to_string())
  })
}

#[test]
fn adt_generation() {
  run_golden_test_dir(function_name!(), &|code| {
    let mut book = do_parse_book(code)?;
    book.generate_scott_adts();
    Ok(book.to_string())
  })
}
