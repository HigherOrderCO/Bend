use hvm_lang::{
  ast::core::show_lnet,
  loader::print_err_reports,
  parser::{parse_definition_book, parse_term},
  run_book,
  to_core::{book_to_hvm_core, term_to_hvm_core},
};
use pretty_assertions::assert_eq;
use std::{collections::HashMap, fs, io::Write, path::Path};
use walkdir::WalkDir;

fn run_single_golden_test(
  path: &Path,
  run: &dyn Fn(&Path, &str) -> anyhow::Result<String>,
) -> anyhow::Result<()> {
  let code = fs::read_to_string(path)?;
  let result = match run(path, &code) {
    Ok(res) => res,
    Err(err) => err.to_string(),
  };
  let golden_path = path.with_extension("golden");
  if let Ok(to_check) = fs::read_to_string(&golden_path) {
    assert_eq!(result, to_check, "Testing file '{}'", path.display());
    Ok(())
  } else {
    let mut file = fs::File::create(golden_path)?;
    file.write_all(result.as_bytes())?;
    Ok(())
  }
}

fn run_golden_test_dir(root: &Path, run: &dyn Fn(&Path, &str) -> anyhow::Result<String>) {
  for entry in WalkDir::new(root).follow_links(true) {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.is_file() && path.extension().map(|x| x == "hvm").unwrap_or(false) {
      eprintln!("running {}", path.display());
      run_single_golden_test(path, run).unwrap();
    }
  }
}

#[test]
fn compile_single_terms() {
  let root = format!("{}/tests/golden_tests/compile_single_terms", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|path, code| {
    let term = parse_term(code).map_err(|errs| {
      print_err_reports(&path.to_string_lossy(), code, errs);
      anyhow::anyhow!("Parsing error")
    })?;
    let net = term_to_hvm_core(term, &HashMap::new())?;
    Ok(show_lnet(&net))
  })
}

#[test]
fn compile_single_files() {
  let root = format!("{}/tests/golden_tests/compile_single_files", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|path, code| {
    let book = parse_definition_book(code).map_err(|errs| {
      print_err_reports(&path.to_string_lossy(), code, errs);
      anyhow::anyhow!("Parsing error")
    })?;
    let core_book = book_to_hvm_core(&book)?;
    Ok(core_book.to_string())
  })
}

#[test]
fn run_single_files() {
  let root = format!("{}/tests/golden_tests/run_single_files", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|path, code| {
    let book = parse_definition_book(code).map_err(|errs| {
      print_err_reports(&path.to_string_lossy(), code, errs);
      anyhow::anyhow!("Parsing error")
    })?;
    let (res, _) = run_book(&book)?;
    Ok(res.to_string())
  })
}
