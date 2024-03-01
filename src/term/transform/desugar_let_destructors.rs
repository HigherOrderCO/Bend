use crate::term::{Book, Name, Pattern, Rule, Term};

impl Book {
  /// Convert let destructor expressions like `let (a, b) = X` into the equivalent match expression.
  pub fn desugar_let_destructors(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.desugar_let_destructors();
      }
    }
  }
}

impl Term {
  pub fn desugar_let_destructors(&mut self) {
    Term::recursive_call(move || match self {
      // Only transform `let`s that are not on variables.
      Term::Let { pat: Pattern::Var(_), .. } => {
        for child in self.children_mut() {
          child.desugar_let_destructors();
        }
      }
      Term::Let { pat, val, nxt } => {
        let pat = std::mem::replace(pat, Pattern::Var(None));
        let mut val = std::mem::take(val);
        let mut nxt = std::mem::take(nxt);

        val.desugar_let_destructors();
        nxt.desugar_let_destructors();

        let rules = vec![Rule { pats: vec![pat], body: *nxt }];

        *self = if let Term::Var { .. } = val.as_ref() {
          Term::Mat { args: vec![*val], rules }
        } else {
          let nam = Name::from("%matched");
          let pat = Pattern::Var(Some(nam.clone()));
          let args = vec![Term::Var { nam }];
          Term::Let { pat, val, nxt: Box::new(Term::Mat { args, rules }) }
        };
      }

      _ => {
        for child in self.children_mut() {
          child.desugar_let_destructors();
        }
      }
    })
  }
}
