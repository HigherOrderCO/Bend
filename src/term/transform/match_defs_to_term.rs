use crate::term::{Book, Definition, Name, Rule, Term};

impl Book {
  /// See [`Definition::convert_match_def_to_term`].
  pub fn convert_match_def_to_term(&mut self) {
    for def in self.defs.values_mut() {
      def.convert_match_def_to_term();
    }
  }
}

impl Definition {
  /// Converts a pattern matching function with multiple rules and args, into a single rule without pattern matching.
  /// Moves the pattern matching of the rules into a complex match expression.
  ///
  /// Preconditions: Rule arities must be correct
  pub fn convert_match_def_to_term(&mut self) {
    let rule = def_rules_to_match(std::mem::take(&mut self.rules));
    self.rules = vec![rule];
  }
}

fn def_rules_to_match(rules: Vec<Rule>) -> Rule {
  let arity = rules[0].arity();

  if arity == 0 {
    // If no args, not pattern matching function, nothings needs to be done.
    rules.into_iter().next().unwrap()
  } else {
    let nams = (0 .. arity).map(|i| Name::new(format!("%x{i}")));

    let args = nams.clone().map(|nam| Term::Var { nam }).collect();
    let mat = Term::Mat { args, rules };
    let body = nams.rfold(mat, |bod, nam| Term::named_lam(nam, bod));
    Rule { pats: vec![], body }
  }
}
