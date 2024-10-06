use crate::{
  fun::{Book, MatchRule, Name, Pattern, Term},
  maybe_grow, AdtEncoding,
};

impl Book {
  /// Encodes pattern matching expressions in the book into their
  /// core form. Must be run after [`Ctr::fix_match_terms`].
  ///
  /// ADT matches are encoded based on `adt_encoding`.
  ///
  /// Num matches are encoded as a sequence of native num matches (on 0 and 1+).
  pub fn encode_matches(&mut self, adt_encoding: AdtEncoding) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.encode_matches(adt_encoding);
      }
    }
  }
}

impl Term {
  pub fn encode_matches(&mut self, adt_encoding: AdtEncoding) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.encode_matches(adt_encoding)
      }

      if let Term::Mat { arg, bnd: _, with_bnd, with_arg, arms } = self {
        assert!(with_bnd.is_empty());
        assert!(with_arg.is_empty());
        let arg = std::mem::take(arg.as_mut());
        let rules = std::mem::take(arms);
        *self = encode_match(arg, rules, adt_encoding);
      } else if let Term::Swt { arg, bnd: _, with_bnd, with_arg, pred, arms } = self {
        assert!(with_bnd.is_empty());
        assert!(with_arg.is_empty());
        let arg = std::mem::take(arg.as_mut());
        let pred = std::mem::take(pred);
        let rules = std::mem::take(arms);
        *self = encode_switch(arg, pred, rules);
      }
    })
  }
}

fn encode_match(arg: Term, rules: Vec<MatchRule>, adt_encoding: AdtEncoding) -> Term {
  match adt_encoding {
    AdtEncoding::Scott => {
      let arms = rules.into_iter().map(|rule| Term::rfold_lams(rule.2, rule.1.into_iter()));
      Term::call(arg, arms)
    }
    AdtEncoding::NumScott => {
      fn make_switches(arms: &mut [Term]) -> Term {
        maybe_grow(|| match arms {
          [] => Term::Err,
          [arm] => Term::lam(Pattern::Var(None), std::mem::take(arm)),
          [arm, rest @ ..] => Term::lam(
            Pattern::Var(Some(Name::new("%tag"))),
            Term::Swt {
              arg: Box::new(Term::Var { nam: Name::new("%tag") }),
              bnd: None,
              with_bnd: vec![],
              with_arg: vec![],
              pred: None,
              arms: vec![std::mem::take(arm), make_switches(rest)],
            },
          ),
        })
      }
      let mut arms =
        rules.into_iter().map(|rule| Term::rfold_lams(rule.2, rule.1.into_iter())).collect::<Vec<_>>();
      let term = if arms.len() == 1 {
        // λx (x λtag switch tag {0: Ctr0; _: * })
        let arm = arms.pop().unwrap();
        let term = Term::Swt {
          arg: Box::new(Term::Var { nam: Name::new("%tag") }),
          bnd: None,
          with_bnd: vec![],
          with_arg: vec![],
          pred: None,
          arms: vec![arm, Term::Era],
        };
        Term::lam(Pattern::Var(Some(Name::new("%tag"))), term)
      } else {
        // λx (x λtag switch tag {0: Ctr0; _: switch tag-1 { ... } })
        make_switches(arms.as_mut_slice())
      };
      Term::call(arg, [term])
    }
  }
}

/// Convert into a sequence of native switches, decrementing by 1 each switch.
/// switch n {0: A; 1: B; _: (C n-2)} converted to
/// switch n {0: A; _: @%x match %x {0: B; _: @n-2 (C n-2)}}
fn encode_switch(arg: Term, pred: Option<Name>, mut rules: Vec<Term>) -> Term {
  // Create the cascade of switches
  let match_var = Name::new("%x");
  let (succ, nums) = rules.split_last_mut().unwrap();
  let last_arm = Term::lam(Pattern::Var(pred), std::mem::take(succ));
  nums.iter_mut().enumerate().rfold(last_arm, |term, (i, rule)| {
    let arms = vec![std::mem::take(rule), term];
    if i == 0 {
      Term::Swt {
        arg: Box::new(arg.clone()),
        bnd: None,
        with_bnd: vec![],
        with_arg: vec![],
        pred: None,
        arms,
      }
    } else {
      let swt = Term::Swt {
        arg: Box::new(Term::Var { nam: match_var.clone() }),
        bnd: None,
        with_bnd: vec![],
        with_arg: vec![],
        pred: None,
        arms,
      };
      Term::lam(Pattern::Var(Some(match_var.clone())), swt)
    }
  })
}
