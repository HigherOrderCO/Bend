use hvm_core::{parse_lnet, show_lnet};
use hvm_lang::{
  compile_book,
  from_core::readback_net,
  loader::display_err_for_text,
  parser::{parse_definition_book, parse_term},
  run_book,
  to_core::term_to_hvm_core,
};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use std::{fs, io::Write, path::Path};
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
  let walker = WalkDir::new(root).sort_by_file_name().max_depth(2).into_iter().filter_entry(|e| {
    let path = e.path();
    if path == root {
      true
    } else if path.is_file() && path.extension().map(|x| x == "hvm").unwrap_or(false) {
      true
    } else if path.is_dir() {
      // We only go inside directories if their name is the name of an enabled feature.
      // To do this, we get the feature name from the directory and check if enabled.
      // This allows us to enable and disable tests based on the default features.
      // TODO: We want to be able to check all tests in one go, enabling and disabling features as necessary.
      if let Some(dir_name) = path.file_name() {
        let feature_name = format!("CARGO_FEATURE_{}", dir_name.to_str().unwrap_or(""));
        std::env::var(feature_name).is_ok()
      } else {
        false
      }
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
fn compile_single_terms() {
  let root = format!("{}/tests/golden_tests/compile_single_terms", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|_, code| {
    let term = parse_term(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    let term = term.sanitize_vars(&Default::default())?;
    let net = term_to_hvm_core(&term)?;
    Ok(show_lnet(&net))
  })
}

#[test]
fn compile_single_files() {
  let root = format!("{}/tests/golden_tests/compile_single_files", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|_, code| {
    let book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    let (core_book, def_names) = compile_book(book)?;
    Ok(core_book.to_string(&def_names))
  })
}

#[test]
fn run_single_files() {
  let root = format!("{}/tests/golden_tests/run_single_files", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|_, code| {
    let book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
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
  let root = format!("{}/tests/golden_tests/readback_lnet", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|_, code| {
    let lnet = parse_lnet(&mut code.chars().peekable());
    let def_names = Default::default();
    let (term, valid) = readback_net(&lnet)?;
    if valid {
      Ok(term.to_string(&def_names))
    } else {
      Ok(format!("Invalid readback:\n{}", term.to_string(&def_names)))
    }
  })
}

#[test]
fn flatten_rules() {
  let root = format!("{}/tests/golden_tests/flatten_rules", env!("CARGO_MANIFEST_DIR"));
  run_golden_test_dir(Path::new(&root), &|_, code| {
    let mut book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    book.flatten_rules();
    Ok(book.to_string())
  })
}
