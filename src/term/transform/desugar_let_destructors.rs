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
    match self {
      Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.desugar_let_destructors();
        snd.desugar_let_destructors();
      }
      Term::Mat { args, rules } => {
        for arg in args {
          arg.desugar_let_destructors();
        }
        for rule in rules {
          rule.body.desugar_let_destructors();
        }
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.desugar_let_destructors();
      }
      Term::Num { .. }
      | Term::Str { .. }
      | Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => (),

      Term::Let { .. } => {
        let Term::Let { pat, val, nxt } = self else { unreachable!() };
        let pat = pat.clone();
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
      Term::Lst { .. } => unreachable!("Should have been desugared already"),
    }
  }
}
