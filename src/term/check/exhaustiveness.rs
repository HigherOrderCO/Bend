use std::collections::HashMap;

use itertools::Itertools;

use crate::term::{Definition, Name, Rule, RulePat};

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
  RulePat::Var(Name(String::from("_")))
}

/// Specializes a row based on the first pattern.
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

fn specialize_matrix(matrix: Matrix, label: Name, size: usize) -> Matrix {
  matrix.iter().map(|row| specialize(row, &label, size)).concat()
}

// applies a substitution based on the var name
fn substitute(name: Name, to: RulePat, from: RulePat) -> RulePat {
  match from {
    RulePat::Ctr(nam, args) => {
      let args = args.iter().map(|arg| substitute(name.clone(), to.clone(), arg.clone())).collect();
      RulePat::Ctr(nam, args)
    }
    RulePat::Var(nam) if nam == name => to,
    RulePat::Var(..) => from,
  }
}

fn idx_to_name(idx: usize) -> Name {
  Name(idx.to_string())
}

fn substitute_list(list: Vec<RulePat>, typ: RulePat) -> RulePat {
  list.into_iter().enumerate().rev().fold(typ, |acc, (idx, rep)| substitute(idx_to_name(idx), rep, acc))
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
  pub ctx_types: HashMap<String, Vec<String>>,
  /// A map from the constructor type to pat types.
  pub ctx_cons: HashMap<String, Vec<RulePat>>,
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
// then the signature is complete
fn is_sig_complete(ctx: &Ctx, type_name: String, names: Vec<String>) -> Completeness {
  if let Some(ctors) = ctx.ctx_types.get(&type_name) {
    let missing: Vec<String> = ctors.iter().filter(|n| !names.contains(n)).cloned().collect();

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
  pub fn type_name(&self) -> Option<(Name, Vec<RulePat>)> {
    match &self.types[0] {
      RulePat::Var(..) => None,
      RulePat::Ctr(name, args) => Some((name.clone(), args.clone())),
    }
  }
}

impl Ctx {
  pub fn specialize_ctr(&self, constructor: &Name, args: Vec<RulePat>, problem: &Problem) -> bool {
    // get the current constructor type
    let constructor_type = self.ctx_cons.get(&constructor.0).expect("Constructor not found in context");

    // specialize the matrix with the constructor_type
    let matrix = specialize_matrix(problem.matrix.clone(), constructor.clone(), constructor_type.len());

    // substitute all vars with the args and get the types of the problem
    let mut typ =
      constructor_type.iter().map(|typ| substitute_list(args.clone(), typ.clone())).collect::<Vec<RulePat>>();
    typ.extend_from_slice(&problem.types[1 ..]);

    // specialize the cases for the current constructor
    let cases = specialize(&problem.case, constructor, constructor_type.len()).concat();

    let mut problem_ = Problem { matrix, case: cases, types: typ };

    useful(self, &mut problem_)
  }
}

// if a signature is complete, we split and specialize the matrix
// into new problems for each constructor of a type
fn split(ctx: &Ctx, typ: &Name, args: Vec<RulePat>, problem: &Problem) -> bool {
  if let Some(typ) = ctx.ctx_types.get(&typ.0) {
    typ.iter().any(|label| ctx.specialize_ctr(&Name(label.clone()), args.clone(), problem))
  } else {
    false
  }
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

pub fn useful(ctx: &Ctx, problem: &mut Problem) -> bool {
  match problem {
    // if the matrix has 0 columns and 0 rows the case is useful
    // to the matrix
    problem if problem.is_empty() => true,

    // if the matrix has 0 columns but more than 0 rows the case is useless
    // to the matrix
    problem if problem.is_complete() => false,

    problem => {
      // take the types of the patterns
      if let Some((name, args)) = problem.type_name() {
        match &problem.case[0] {
          // if it is a wildcard/var
          RulePat::Var(..) => {
            if is_wildcard_matrix(&problem.matrix) {
              // if all patterns of the first column are wildcards then
              // get the default matrix and check again
              useful(ctx, &mut default_matrix(problem))
            } else {
              let cons = problem.matrix.iter().map(|row| get_pat_ctr_name(row[0].clone())).collect();
              if let Some(names) = cons {
                match is_sig_complete(ctx, name.0.clone(), names) {
                  // if the signature is complete then split based on the type
                  Completeness::Complete(_) => split(ctx, &name, args, problem),
                  // if the signature of the first pattern is incomplete then get the default matrix
                  // and check again
                  Completeness::Incomplete(_) => useful(ctx, &mut default_matrix(problem)),
                }
              } else {
                false
              }
            }
          }
          // if it is a constructor we specialize this constructor case
          RulePat::Ctr(nam_, ..) => ctx.specialize_ctr(&nam_, args, &problem),
        }
      } else {
        false
      }
    }
  }
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use crate::term::{
    check::exhaustiveness::{useful, wildcard, Ctx, Problem},
    Name,
    RulePat::{self, *},
  };

  fn ctr(name: &str) -> RulePat {
    Ctr(Name(String::from(name)), vec![])
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
      ctx_types: HashMap::from([("Bool".to_string(), vec!["T".to_string(), "F".to_string()])]),
      ctx_cons: HashMap::from([("T".to_string(), vec![]), ("F".to_string(), vec![])]),
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
      ctx_types: HashMap::from([("Bool".to_string(), vec!["T".to_string(), "F".to_string()])]),
      ctx_cons: HashMap::from([("T".to_string(), vec![]), ("F".to_string(), vec![])]),
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
      ctx_types: HashMap::from([("Bool".to_string(), vec!["T".to_string(), "F".to_string()])]),
      ctx_cons: HashMap::from([("T".to_string(), vec![]), ("F".to_string(), vec![])]),
    };

    let is_useful = useful(&ctx, &mut problem);
    assert_eq!(true, is_useful);
  }
}
