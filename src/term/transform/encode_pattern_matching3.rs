use crate::term::{Book, Name, RulePat};
use std::collections::HashSet;

#[derive(Debug)]
/// A tree that maps a sequence of patterns to the selected rule
enum PatDecisionTree {
  //
  Branch(Vec<(Name, PatDecisionTree)>),
  Leaf(RuleIdx),
}

/// The pattern matrix has the patterns for some rules plus the index that rule corresponds to in the original def.
#[derive(Debug, Clone)]
struct Problem {
  rows: Vec<Row>,
}
struct Row {
  pats: Vec<RulePat>,
  rule_idx: RuleIdx,
}
type RuleIdx = usize;
type Actions = Vec<RuleIdx>;

impl Book {
  fn compile_defs(&self) {
    for def in self.defs.values() {
      let problem = Problem {
        rows: def.rules.iter().enumerate().map(|(i, r)| Row { pats: r.pats.clone(), rule_idx: i }).collect(),
      };
      let cmp = problem.compile();
      println!("{cmp:?}");
    }
  }
}

impl Problem {
  fn compile(mut self) -> PatDecisionTree {
    let is_fst_row_irrefutable = self.rows[0].pats.iter().all(|p| matches!(p, RulePat::Var(..)));
    if is_fst_row_irrefutable {
      // Found the rule for this path
      PatDecisionTree::Leaf(self.rows[0].rule_idx)
    } else {
      let refutable_col = self.find_refutable_col();
      self.swap_col(0, refutable_col);
      let ctrs = self.ctrs_used_in_col(0);

      // TODO: check the signature completeness
      let mut switches = Vec::new();
      for ctr in ctrs {
        let problem = self.specialize(&ctr);
        let result = problem.compile();
        switches.push((ctr, result))
      }

      PatDecisionTree::Branch(switches)
    }
  }

  // Returns the subproblem after matching `ctr` on the first argument.
  fn specialize(&self, ctr: &Name) -> Problem {
    let mut problem = Problem::default();
    for row in self.rows.iter() {
      let specialized_row = match &row.pats[0] {
        RulePat::Var(_) => Some(row.pats[1 ..].to_vec()),
        RulePat::Ctr(nam, ..) if nam == ctr => Some(row.pats[1 ..].to_vec()),
        RulePat::Ctr(..) => None,
      };
      if let Some(row) = specialized_row {
        problem.matrix.push(row);
        problem.actions.push(self.actions[idx]);
      }
    }
    problem
  }

  fn specialize_row(&self, idx: usize, ctr: &Name) -> Option<Row> {
    match &self.matrix[idx][0] {
      RulePat::Var(_) => Some(self.matrix[idx][1 ..].to_vec()),
      RulePat::Ctr(nam, ..) if nam == ctr => Some(self.matrix[idx][1 ..].to_vec()),
      RulePat::Ctr(..) => None,
    }
  }

  fn swap_col(&mut self, col1: usize, col2: usize) {
    for row in &mut self.matrix {
      row.swap(col1, col2);
    }
  }

  fn is_col_refutable(&self, col: usize) -> bool {
    self.matrix.iter().any(|r| matches!(r[col], RulePat::Ctr(..)))
  }

  fn ctrs_used_in_col(&self, col: usize) -> HashSet<Name> {
    let mut ctrs = HashSet::new();
    for row in self.matrix.iter() {
      match &row[col] {
        RulePat::Var(_) => {}
        RulePat::Ctr(nam, _) => {
          ctrs.insert(nam.clone());
        }
      }
    }
    ctrs
  }

  fn find_refutable_col(&self) -> usize {
    let cols = self.matrix[0].len();

    for idx in 0 .. cols {
      if self.is_col_refutable(idx) {
        return idx;
      }
    }
    panic!()
  }
}
