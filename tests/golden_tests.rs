use hvmc::ast::{parse_net, show_book, show_net};
use hvml::{
  compile_book,
  net::{hvmc_to_net::hvmc_to_net, net_to_hvmc::net_to_hvmc},
  run_book,
  term::{
    net_to_term::net_to_term_non_linear,
    parser::{parse_definition_book, parse_term},
    term_to_compat_net, Book, DefId, Term,
  },
};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use std::{
  fs,
  io::Write,
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

fn run_single_golden_test(
  path: &Path,
  run: &dyn Fn(&Path, &str) -> Result<String, String>,
) -> Result<(), String> {
  let code = fs::read_to_string(path).map_err(|e| e.to_string())?;
  let result = match run(path, &code) {
    Ok(res) => res,
    Err(err) => err.to_string(),
  };
  let golden_path = path.with_extension("golden");
  if let Ok(to_check) = fs::read_to_string(&golden_path) {
    assert_eq!(result, to_check, "Testing file '{}'", path.display());
    Ok(())
  } else {
    let mut file = fs::File::create(golden_path).map_err(|e| e.to_string())?;
    file.write_all(result.as_bytes()).map_err(|e| e.to_string())?;
    Ok(())
  }
}

fn run_golden_test_dir(test_name: &str, run: &dyn Fn(&Path, &str) -> Result<String, String>) {
  let root = PathBuf::from(format!(
    "{}/tests/golden_tests/{}",
    env!("CARGO_MANIFEST_DIR"),
    test_name.rsplit_once(":").unwrap().1
  ));
  let walker = WalkDir::new(&root).sort_by_file_name().max_depth(2).into_iter().filter_entry(|e| {
    let path = e.path();
    if path == root {
      true
    } else if path.is_file() && path.extension().map(|x| x == "hvm").unwrap_or(false) {
      true
    } else {
      false
    }
  });
  for entry in walker {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.is_file() {
      eprintln!("running {}", path.display());
      run_single_golden_test(path, run).unwrap();
    }
  }
}

#[test]
fn compile_term() {
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut term = do_parse_term(code)?;
    term.check_unbound_vars()?;
    term.make_var_names_unique();
    term.linearize_vars();
    let compat_net = term_to_compat_net(&term);
    let net = net_to_hvmc(&compat_net, &|def_id| def_id.to_internal())?;
    Ok(show_net(&net))
  })
}

#[test]
fn compile_file() {
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut book = do_parse_book(code)?;
    let (compiled, _) = compile_book(&mut book)?;
    Ok(show_book(&compiled))
  })
}

#[test]
fn run_single_files() {
  run_golden_test_dir(function_name!(), &|_, code| {
    let book = do_parse_book(code)?;
    // 1 million nodes for the test runtime. Smaller doesn't seem to make it any faster
    let (res, def_names, info) = run_book(book, 1 << 20)?;
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
  run_golden_test_dir(function_name!(), &|_, code| {
    let net = do_parse_net(code)?;
    let book = Book::default();
    let compat_net = hvmc_to_net(&net, &|val| DefId::from_internal(val));
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
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut book = do_parse_book(code)?;
    book.flatten_rules();
    Ok(book.to_string())
  })
}

#[test]
fn adt_generation() {
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut book = do_parse_book(code)?;
    book.generate_scott_adts();
    Ok(book.to_string())
  })
}
