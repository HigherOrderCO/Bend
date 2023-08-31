use hvm_lang::{parser::parse_term, to_core::term_to_hvm_core};
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn check_single_term_compilation(code: &str, expected: &str) {
  let term = parse_term(code).unwrap();
  let net = term_to_hvm_core(&term, &HashMap::new()).unwrap();
  assert_eq!(net.to_string(), expected)
}

#[test]
fn single_def_terms() {
  let terms = [
    ("id", "λa a", "$ (0 x0 x0)"),
    ("dup apply", "λa dup a1 a2 = a; (a1 a2)", "$ (0 (1 (0 x0 x1) x0) x1)"),
    ("erased dup", "λa dup a1 a2 = a; a1", "$ (0 (1 x0 *) x0)"),
    ("c_z", "λs λz z", "$ (0 * (0 x0 x0))"),
    (
      "c_s",
      "λk λs dup s0 s1 = s; λz (s0 ((k s1) z))",
      "$ (0 (0 x0 (0 x1 x2)) (0 (1 (0 x2 x3) x0) (0 x1 x3)))",
    ),
  ];
  for (_name, code, expected) in terms {
    check_single_term_compilation(code, expected)
  }
}
