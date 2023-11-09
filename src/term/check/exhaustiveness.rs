use std::collections::BTreeMap;

use itertools::Itertools;

use crate::term::{Adt, Book, Definition, Name, Rule, RulePat};

use super::type_check::{DefinitionTypes, Type};

type Row = Vec<RulePat>;
type Matrix = Vec<Row>;

impl Rule {
  pub fn get_row(&self) -> Row {
    self.pats.clone()
  }
}

impl Definition {
  pub fn get_matrix(&self) -> Matrix {
    self.rules.iter().map(Rule::get_row).collect()
  }
}

fn wildcard() -> RulePat {
  RulePat::Var(Name::new("_"))
}

/// Specializes a row based on the first pattern to discover if it is
/// exhaustive for the given label.
fn specialize(row: &Row, label: &Name, size: usize) -> Matrix {
  let first = &row[0];
  match first {
    RulePat::Var(..) => {
      // add wildcards based on the constructor size and extend with the
      // tail of the row
      let mut xs = vec![wildcard(); size];
      xs.extend(row[1 ..].to_vec());
      vec![xs]
    }
    RulePat::Ctr(nam, ..) => match nam {
      // if the name matches the label return the tail of the row else remove the row
      name if name == label => vec![row[1 ..].to_vec()],
      _ => vec![],
    },
  }
}

fn specialize_matrix(matrix: &Matrix, label: &Name, size: usize) -> Matrix {
  matrix.iter().map(|row| specialize(row, label, size)).concat()
}

pub struct Problem {
  /// A matrix containing the patterns.
  pub matrix: Matrix,
  /// The cases we want to check.
  pub case: Vec<RulePat>,
  /// The types of a row in the matrix.
  pub types: Vec<RulePat>,
}

pub struct Ctx {
  /// A map from the ADT type to its constructors.
  pub ctx_types: BTreeMap<Name, Adt>,
  /// A map from the constructor to its arity.
  pub ctx_cons: BTreeMap<Name, usize>,
}

#[derive(Clone, Debug)]
pub enum Completeness {
  Complete(Vec<String>),
  Incomplete(Vec<String>),
}

fn get_pat_ctr_name(pat: RulePat) -> Option<String> {
  match pat {
    RulePat::Var(..) => None,
    RulePat::Ctr(Name(nam), ..) => Some(nam),
  }
}

// with the completeness of the signature we discover if we
// need to specialize or not the matrix.
// if the column contains all names in a set of constructors
// then the signature is complete.
fn is_sig_complete(ctx: &Ctx, type_name: &Name, names: Vec<String>) -> Completeness {
  if let Some(ctors) = ctx.ctx_types.get(type_name) {
    let ctrs = ctors.ctrs.keys().clone();
    let missing = ctrs.map(|n| n.0.clone()).filter(|n| !names.contains(&n)).collect::<Vec<_>>();

    if missing.is_empty() { Completeness::Complete(names) } else { Completeness::Incomplete(missing) }
  } else {
    Completeness::Incomplete(names)
  }
}

impl Problem {
  pub fn is_empty(&self) -> bool {
    self.matrix.is_empty()
  }

  pub fn is_complete(&self) -> bool {
    self.matrix.iter().any(Vec::is_empty)
  }
}

fn default_row(row: &Row) -> Matrix {
  match &row[0] {
    // if it is a wildcard, remove the first pat of the row
    RulePat::Var(..) => vec![row[1 ..].to_vec()],
    // if it is a constructor, remove the row
    RulePat::Ctr(..) => vec![],
  }
}

fn default_matrix(problem: &mut Problem) -> Problem {
  let matrix = problem.matrix.iter().map(default_row).concat();
  let types = problem.types[1 ..].to_vec();
  let case = default_row(&problem.case).concat();

  Problem { matrix, case, types }
}

impl Problem {
  pub fn type_name(&self) -> Option<Name> {
    match &self.types[0] {
      RulePat::Var(..) => None,
      RulePat::Ctr(name, ..) => Some(name.clone()),
    }
  }
}

impl Ctx {
  pub fn specialize_ctr(&self, constructor: &Name, problem: &Problem) -> bool {
    // get the current constructor type
    let cons_arity = self.ctx_cons.get(constructor).expect("Constructor not found in context");

    // specialize the matrix with the constructor_type
    let matrix = specialize_matrix(&problem.matrix, constructor, *cons_arity);

    let types = problem.types[1 ..].to_vec();

    // specialize the cases for the current constructor
    let cases = specialize(&problem.case, constructor, *cons_arity).concat();

    let mut problem_ = Problem { matrix, case: cases, types };

    useful(self, &mut problem_)
  }
}

// if a signature is complete, we split and specialize the matrix
// into new problems for each constructor of a type
fn split(ctx: &Ctx, typ: &Name, problem: &Problem) -> bool {
  let adt = ctx.ctx_types.get(typ).expect("Type not found in context");
  adt.ctrs.iter().any(|(label, _)| ctx.specialize_ctr(label, problem))
}

fn is_wildcard(pat: &RulePat) -> bool {
  matches!(pat, RulePat::Var(..))
}

fn is_wildcard_row(row: &Row) -> bool {
  row.first().is_some_and(is_wildcard)
}

fn is_wildcard_matrix(matrix: &Matrix) -> bool {
  matrix.iter().all(is_wildcard_row)
}

fn useful(ctx: &Ctx, problem: &mut Problem) -> bool {
  match problem {
    // if the matrix has 0 columns and 0 rows the case is useful
    // to the matrix
    problem if problem.is_empty() => true,

    // if the matrix has 0 columns but more than 0 rows the case is useless
    // to the matrix
    problem if problem.is_complete() => false,

    problem => {
      // take the types of the patterns
      match problem.type_name() {
        None => false,
        Some(name) => {
          match &problem.case[0] {
            // if it is a wildcard/var
            RulePat::Var(..) => {
              if is_wildcard_matrix(&problem.matrix) {
                // if all patterns of the first column are wildcards then
                // get the default matrix and check again
                useful(ctx, &mut default_matrix(problem))
              } else {
                let names = problem
                  .matrix
                  .iter()
                  .map(|row| get_pat_ctr_name(row[0].clone()))
                  .collect::<Option<Vec<String>>>()
                  .unwrap_or_default();

                match is_sig_complete(ctx, &name, names) {
                  // if the signature is complete then split based on the type
                  Completeness::Complete(_) => split(ctx, &name, problem),
                  // if the signature of the first pattern is incomplete then get the default matrix
                  // and check again
                  Completeness::Incomplete(_) => useful(ctx, &mut default_matrix(problem)),
                }
              }
            }
            // if it is a constructor we specialize this constructor case
            RulePat::Ctr(nam_, ..) => ctx.specialize_ctr(&nam_, &problem),
          }
        }
      }
    }
  }
}

impl Book {
  pub fn check_exhaustiveness(&self, def_types: &DefinitionTypes) -> anyhow::Result<()> {
    for def in self.defs.values() {
      if let Some(def_types) = def_types.get(&def.def_id) {
        // get the type of each argument
        let types = def_types
          .into_iter()
          .map(|t| match t {
            Type::Any => wildcard(),
            Type::Adt(name) => RulePat::Ctr(name.clone(), vec![]),
          })
          .collect::<Vec<RulePat>>();

        let matrix = def.get_matrix();
        let def_arity = def.arity();
        // this is the default case to check if a definition is exhaustive
        let case = vec![wildcard(); def_arity];

        // ctx_cons, ex: [Ctr("Nil", 0), Ctr("Cons", 2)]
        let mut ctx_cons = BTreeMap::<Name, usize>::new();
        for t in &types {
          match t {
            RulePat::Var(..) => {}
            RulePat::Ctr(nam, ..) => {
              let adt = self.adts.get(nam).cloned().unwrap();
              ctx_cons.extend(adt.ctrs);
            }
          }
        }

        let mut problem = Problem { matrix, case, types };

        let ctx_types = self.adts.clone();
        let ctx = Ctx { ctx_types, ctx_cons };

        // if the case is useful that means that the rule is not exhaustive
        if useful(&ctx, &mut problem) {
          let def_name = self.def_names.map.get_by_left(&def.def_id).unwrap();

          return Err(anyhow::anyhow!("The definition '{def_name}' is not exhaustive."));
        }
      }
    }

    Ok(())
  }
}

#[cfg(test)]
mod test {
  use std::collections::BTreeMap;

  use indexmap::IndexMap;

  use crate::term::{
    check::exhaustiveness::{useful, wildcard, Ctx, Problem},
    parser::parse_definition_book,
    Adt, Name,
    RulePat::{self, *},
  };

  fn ctr(name: &str) -> RulePat {
    Ctr(Name(String::from(name)), vec![])
  }

  fn bool_ctx() -> BTreeMap<Name, Adt> {
    BTreeMap::from([(Name::new("Bool"), Adt {
      ctrs: IndexMap::from([(Name::new("T"), 0), (Name::new("F"), 0)]),
    })])
  }

  #[test]
  fn definition_bar_is_exhaustive() {
    let code = r"
    data List
      = (Cons x xs)
      | Nil
    data Bool = T | F

    bar (Cons (T) xs) = 2
    bar (Cons (F) xs) = 1
    bar (Nil)         = 0
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let ok = book.check_exhaustiveness(&def_types).unwrap();
        assert_eq!((), ok)
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  fn definition_bar_flattened_is_not_exhaustive() {
    let code = r"
    data List
      = (Cons x xs)
      | Nil
    data Bool = T | F

    bar (Cons (T) xs) = (Cons (F) (Nil))
    bar (Nil)         = (Nil)
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let err = book.check_exhaustiveness(&def_types).unwrap_err();
        assert_eq!(format!("{}", err), "The definition 'bar$0$' is not exhaustive.")
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  fn definition_foo_is_not_exhaustive() {
    let code = r"
    data Bool = T | F

    foo (T) = 0
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let err = book.check_exhaustiveness(&def_types).unwrap_err();
        assert_eq!(format!("{}", err), "The definition 'foo' is not exhaustive.")
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  fn definition_and_is_not_exhaustive() {
    let code = r"
    data Bool = T | F

    and (T) (T) = (T)
    and _   (F) = (F)
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let err = book.check_exhaustiveness(&def_types).unwrap_err();
        assert_eq!(format!("{}", err), "The definition 'and' is not exhaustive.")
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  fn definition_and_is_exhaustive() {
    let code = r"
    data Bool = T | F

    and (T) (T) = (T)
    and _   _   = (F)
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let ok = book.check_exhaustiveness(&def_types).unwrap();
        assert_eq!((), ok)
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  fn definitions_are_exhaustive() {
    let code = r"
    data Bool = T | F

    not (T) = (F)
    not _   = (T)

    and (T) (T) = (T)
    and _   _   = (F)
    ";
    let book = parse_definition_book(code);
    match book {
      Ok(mut book) => {
        book.flatten_rules();
        let def_types = book.infer_def_types().unwrap();
        let ok = book.check_exhaustiveness(&def_types).unwrap();
        assert_eq!((), ok)
      }
      Err(_) => assert!(false),
    }
  }

  #[test]
  #[rustfmt::skip]
  // All patterns are covered, wildcard should not be useful.
  fn wildcard_is_not_useful_bool_bool() {
    let matrix = vec![
      vec![ctr("T"), ctr("T")],
      vec![ctr("T"), ctr("F")],
      vec![ctr("F"), ctr("F")],
      vec![ctr("F"), ctr("T")],
    ];
    let case = vec![wildcard(), wildcard()];
    let types =
      vec![ctr("Bool"), ctr("Bool")];
    let mut problem = Problem { matrix, case, types };

    let ctx = Ctx {
      ctx_types: bool_ctx(),
      ctx_cons: BTreeMap::from([(Name::new("T"), 0), (Name::new("F"), 0)]),
    };

    let is_useful = useful(&ctx, &mut problem);
    assert_eq!(false, is_useful);
  }

  #[test]
  #[rustfmt::skip]
  // All patterns are covered, wildcard should not be useful.
  fn wildcard_is_not_useful_bool() {
    let matrix = vec![
      vec![ctr("T")],
      vec![ctr("F")],
    ];
    let case = vec![wildcard()];
    let types = vec![ctr("Bool")];
    let mut problem = Problem { matrix, case, types };

    let ctx = Ctx {
      ctx_types: bool_ctx(),
      ctx_cons: BTreeMap::from([(Name::new("T"), 0), (Name::new("F"), 0)]),
    };

    let is_useful = useful(&ctx, &mut problem);
    assert_eq!(false, is_useful);
  }

  #[test]
  #[rustfmt::skip]
  // Missing the F pattern, wildcard should be useful.
  fn wildcard_is_useful_bool() {
    let matrix = vec![
      vec![ctr("T")],
    ];
    let case = vec![wildcard()];
    let types = vec![ctr("Bool")];
    let mut problem = Problem { matrix, case, types };

    let ctx = Ctx {
      ctx_types: bool_ctx(),
      ctx_cons: BTreeMap::from([(Name::new("T"), 0), (Name::new("F"), 0)]),
    };

    let is_useful = useful(&ctx, &mut problem);
    assert_eq!(true, is_useful);
  }
}
