use std::collections::HashMap;

use itertools::Itertools;

use crate::term::{Definition, Name, Rule, RulePat};

#[derive(Clone, Debug)]
pub enum Pat {
  Wildcard,
  Constructor(Name, Vec<Pat>),
}

impl From<RulePat> for Pat {
  fn from(value: RulePat) -> Self {
    match value {
      RulePat::Var(_) => Self::Wildcard,
      RulePat::Ctr(nam, rest) => Self::Constructor(nam, rest.into_iter().map(Self::from).collect()),
    }
  }
}

impl Rule {
  pub fn get_row(&self) -> Vec<Pat> {
    self.pats.iter().map(|pat| From::from(pat.clone())).collect()
  }
}

impl Definition {
  pub fn get_matrix(&self) -> Vec<Vec<Pat>> {
    self.rules.iter().map(Rule::get_row).collect()
  }
}

pub fn specialize(row: &Vec<Pat>, label: &Name, size: usize) -> Vec<Vec<Pat>> {
  let first = &row[0];
  match first {
    Pat::Wildcard => {
      let mut xs = vec![Pat::Wildcard; size];
      xs.extend(row[1 ..].to_vec());
      vec![xs]
    }
    Pat::Constructor(nam, args) => match nam {
      name if name == label => {
        let mut xs = args.to_vec();
        xs.extend(row[1 ..].to_vec());
        vec![xs]
      }
      _ => vec![],
    },
  }
}

pub fn specialize_matrix(matrix: Vec<Vec<Pat>>, label: Name, size: usize) -> Vec<Vec<Pat>> {
  matrix.iter().map(|row| specialize(row, &label, size)).concat()
}

#[derive(Clone, Debug)]
pub enum PatType {
  Var(usize),
  Ctr(Name, Vec<PatType>),
}

pub fn substitute(name: usize, replace: PatType, pat_type: PatType) -> PatType {
  match pat_type {
    PatType::Ctr(nam, args) => {
      let args = args.iter().map(|a| substitute(name, replace.clone(), a.clone())).collect();
      PatType::Ctr(nam, args)
    }
    PatType::Var(nam) if nam == name => replace,
    PatType::Var(n) => PatType::Var(n),
  }
}

pub fn substitute_list(rep: Vec<PatType>, typ: PatType) -> PatType {
  rep.into_iter().enumerate().rev().fold(typ, |acc, (idx, rep)| substitute(idx, rep, acc))
}

pub struct Problem {
  pub matrix: Vec<Vec<Pat>>,
  pub case: Vec<Pat>,
  pub types: Vec<PatType>,
}

pub struct Ctx {
  pub ctx_types: HashMap<String, Vec<String>>,
  pub ctx_cons: HashMap<String, Vec<PatType>>,
}

#[derive(Clone, Debug)]
pub enum Completeness {
  Complete(Vec<String>),
  Incomplete(Vec<String>),
}

pub fn get_pat_cons(pat: Pat) -> Option<String> {
  match pat {
    Pat::Wildcard => None,
    Pat::Constructor(Name(nam), _) => Some(nam),
  }
}

pub fn is_sig_complete(ctx: &Ctx, type_name: String, names: Vec<String>) -> Completeness {
  if let Some(names_) = ctx.ctx_types.get(&type_name) {
    let missing: Vec<String> = names_.iter().filter(|n| !names.contains(n)).cloned().collect();

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

pub fn default_row(row: &Vec<Pat>) -> Vec<Vec<Pat>> {
  let first = &row[0];
  match first {
    Pat::Wildcard => vec![row[1 ..].to_vec()],
    Pat::Constructor(..) => vec![],
  }
}

pub fn default_matrix(problem: &mut Problem) -> Problem {
  let matrix = problem.matrix.iter().map(default_row).concat();
  let types = problem.types[1 ..].to_vec();
  let case = default_row(&problem.case).concat();

  Problem { matrix, case, types }
}

impl Problem {
  pub fn type_name(&self) -> (Name, Vec<PatType>) {
    match self.types.first() {
      Some(first_type) => match first_type {
        PatType::Var(_) => panic!("This should not exist here"),
        PatType::Ctr(name, args) => (name.clone(), args.clone()),
      },
      None => panic!("Empty problem types {:#?}, {:#?}", self.types, self.matrix),
    }
  }
}

impl Ctx {
  pub fn specialize_ctr(&self, constructor: &Name, args: Vec<PatType>, problem: &Problem) -> bool {
    let constructor_ty = self.ctx_cons.get(&constructor.0).expect("Constructor not found in context");

    let matrix = specialize_matrix(problem.matrix.clone(), constructor.clone(), constructor_ty.len());

    let mut typ =
      constructor_ty.iter().map(|c| substitute_list(args.clone(), c.clone())).collect::<Vec<PatType>>();
    typ.extend_from_slice(&problem.types[1 ..]);

    let cases = specialize(&problem.case, constructor, constructor_ty.len()).concat();

    let mut problem_ = Problem { matrix, case: cases, types: typ };

    useful(self, &mut problem_)
  }
}

pub fn split(ctx: &Ctx, typ: &Name, args: Vec<PatType>, problem: &Problem) -> bool {
  if let Some(typ) = ctx.ctx_types.get(&typ.0) {
    typ.iter().any(|label| ctx.specialize_ctr(&Name(label.clone()), args.clone(), problem))
  } else {
    false
  }
}

pub fn is_wildcard(pat: &Pat) -> bool {
  match pat {
    Pat::Wildcard => true,
    _ => false,
  }
}

pub fn is_wildcard_row(row: &Vec<Pat>) -> bool {
  row.first().is_some_and(is_wildcard)
}

pub fn is_wildcard_matrix(matrix: &Vec<Vec<Pat>>) -> bool {
  matrix.iter().all(is_wildcard_row)
}

pub fn useful(ctx: &Ctx, problem: &mut Problem) -> bool {
  match problem {
    problem if problem.is_empty() => true,
    problem if problem.is_complete() => false,
    problem => {
      let (name, args) = problem.type_name();
      match &problem.case[0] {
        Pat::Wildcard => {
          if is_wildcard_matrix(&problem.matrix) {
            useful(ctx, &mut default_matrix(problem))
          } else {
            let cons = problem.matrix.iter().map(|row| get_pat_cons(row[0].clone())).collect();
            if let Some(names) = cons {
              match is_sig_complete(ctx, name.0.clone(), names) {
                Completeness::Complete(_) => split(ctx, &name, args, problem),
                Completeness::Incomplete(_) => useful(ctx, &mut default_matrix(problem)),
              }
            } else {
              false
            }
          }
        }
        Pat::Constructor(nam_, _) => ctx.specialize_ctr(&nam_, args, &problem),
      }
    }
  }
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use crate::term::{
    check::exhaustiveness::{useful, Ctx, PatType, Problem},
    Name,
  };

  use super::Pat;

  #[test]
  fn test() {
    let matrix = vec![
      vec![
        Pat::Constructor(Name(String::from("T")), vec![]),
        Pat::Constructor(Name(String::from("T")), vec![]),
      ],
      vec![
        Pat::Constructor(Name(String::from("T")), vec![]),
        Pat::Constructor(Name(String::from("F")), vec![]),
      ],
      vec![
        Pat::Constructor(Name(String::from("F")), vec![]),
        Pat::Constructor(Name(String::from("F")), vec![]),
      ],
      vec![
        Pat::Constructor(Name(String::from("F")), vec![]),
        Pat::Constructor(Name(String::from("T")), vec![]),
      ],
    ];
    let case = vec![Pat::Wildcard, Pat::Wildcard];
    let types =
      vec![PatType::Ctr(Name("Bool".to_string()), vec![]), PatType::Ctr(Name("Bool".to_string()), vec![])];
    let mut problem = Problem { matrix, case, types };

    let ctx = Ctx {
      ctx_types: HashMap::from([("Bool".to_string(), vec!["T".to_string(), "F".to_string()])]),
      ctx_cons: HashMap::from([("T".to_string(), vec![]), ("F".to_string(), vec![])]),
    };

    let is_useful = useful(&ctx, &mut problem);

    println!("{:#?}", is_useful);
  }
}
