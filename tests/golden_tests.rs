use hvm_lang::{
  ast::{DefId, DefinitionBook},
  compile_book,
  from_core::readback_net,
  loader::{display_err_for_text, display_miette_err},
  parser::{parse_definition_book, parse_term},
  run_book,
  to_core::term_to_hvm_core,
};
use hvmc::{parse_lnet, show_lnet, Val};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use std::{
  fs,
  io::Write,
  path::{Path, PathBuf},
};
use stdext::function_name;
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

fn run_golden_test_dir(test_name: &str, run: &dyn Fn(&Path, &str) -> anyhow::Result<String>) {
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
  run_golden_test_dir(function_name!(), &|_, code| {
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
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    let core_book = compile_book(&mut book)?;
    Ok(core_book.to_string(&book.def_names))
  })
}

#[test]
fn run_single_files() {
  run_golden_test_dir(function_name!(), &|_, code| {
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
  run_golden_test_dir(function_name!(), &|_, code| {
    let lnet = parse_lnet(&mut code.chars().peekable()).map_err(|e| anyhow::anyhow!(e))?;
    let book = DefinitionBook::default();
    let (term, valid) = readback_net(&lnet, &book)?;
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
    let mut book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    book.flatten_rules();
    Ok(book.to_string())
  })
}

#[test]
fn type_inference() {
  run_golden_test_dir(function_name!(), &|_, code| {
    let mut book = parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_err_for_text(e)).join("\n");
      anyhow::anyhow!(msg)
    })?;
    book.check_rule_arities()?;
    book.flatten_rules();
    let (adts, def_types) = book.get_types_from_patterns()?;
    let mut out = String::new();
    out.push_str("Adts: [\n");
    for (adt_id, adt) in adts.iter().sorted_by_key(|x| x.0) {
      out.push_str(&format!("  {adt_id} => {adt}\n"));
    }
    out.push_str("]\nTypes: [\n");
    for (def_id, def_types) in def_types.iter().enumerate() {
      out.push_str(&format!(
        "  {} => [{}]\n",
        book.def_names.name(&DefId::from(def_id as Val)).unwrap(),
        def_types.iter().map(|x| format!("{x:?}")).join(", "),
      ));
    }
    out.push_str("]");
    Ok(out)
  })
}

#[test]
fn error_outputs() {
  let _ = miette::set_hook(Box::new(|_| Box::new(miette::JSONReportHandler::new())));

  run_golden_test_dir(function_name!(), &|path, code| {
    let path = Path::new(path.file_name().unwrap());

    parse_definition_book(code).map_err(|errs| {
      let msg = errs.into_iter().map(|e| display_miette_err(e, path, code)).join("\n");
      println!("{}", msg);
      anyhow::anyhow!(msg)
    })?;

    Ok(String::new())
  })
}
