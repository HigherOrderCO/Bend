use crate::{
  maybe_grow,
  term::{AdtEncoding, Book, Constructors, MatchRule, Name, Pattern, Tag, Term},
};

impl Book {
  /// Encodes pattern matching expressions in the book into their
  /// native/core form. Must be run after [`Ctr::fix_match_terms`].
  ///
  /// ADT matches are encoded based on `adt_encoding`.
  ///
  /// Num matches are encoded as a sequence of native num matches (on 0 and 1+).
  ///
  /// Var and pair matches become a let expression.
  pub fn encode_matches(&mut self, adt_encoding: AdtEncoding) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.encode_matches(&self.ctrs, adt_encoding);
      }
    }
  }
}

impl Term {
  pub fn encode_matches(&mut self, ctrs: &Constructors, adt_encoding: AdtEncoding) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.encode_matches(ctrs, adt_encoding)
      }

      if let Term::Mat { arg, bnd: _, with, arms: rules } = self {
        assert!(with.is_empty());
        let arg = std::mem::take(arg.as_mut());
        let rules = std::mem::take(rules);
        *self = encode_match(arg, rules, ctrs, adt_encoding);
      } else if let Term::Swt { arg, bnd: _, with, pred, arms: rules } = self {
        assert!(with.is_empty());
        let arg = std::mem::take(arg.as_mut());
        let pred = std::mem::take(pred);
        let rules = std::mem::take(rules);
        *self = encode_switch(arg, pred, rules);
      }
    })
  }
}

fn encode_match(arg: Term, rules: Vec<MatchRule>, ctrs: &Constructors, adt_encoding: AdtEncoding) -> Term {
  let adt_nam = ctrs.get(rules[0].0.as_ref().unwrap()).unwrap();

  // ADT Encoding depends on compiler option
  let tag = match adt_encoding {
    AdtEncoding::Scott => Tag::Static,
    AdtEncoding::TaggedScott => Tag::adt_name(adt_nam),
  };
  let mut arms = vec![];
  for rule in rules.into_iter() {
    let body =
      rule.1.iter().cloned().rfold(rule.2, |bod, nam| Term::tagged_lam(tag.clone(), Pattern::Var(nam), bod));
    arms.push(body);
  }
  Term::tagged_call(tag.clone(), arg, arms)
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
      Term::Swt { arg: Box::new(arg.clone()), bnd: None, with: vec![], pred: None, arms }
    } else {
      let swt = Term::Swt {
        arg: Box::new(Term::Var { nam: match_var.clone() }),
        bnd: None,
        with: vec![],
        pred: None,
        arms,
      };
      Term::lam(Pattern::Var(Some(match_var.clone())), swt)
    }
  })
}
