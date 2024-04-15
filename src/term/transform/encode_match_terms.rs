use crate::{
  maybe_grow,
  term::{AdtEncoding, Adts, Book, Constructors, MatchRule, Name, Tag, Term},
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
        rule.body.encode_matches(&self.ctrs, &self.adts, adt_encoding);
      }
    }
  }
}

impl Term {
  pub fn encode_matches(&mut self, ctrs: &Constructors, adts: &Adts, adt_encoding: AdtEncoding) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.encode_matches(ctrs, adts, adt_encoding)
      }

      if let Term::Mat { arg, bnd, with, arms: rules } = self {
        assert!(with.is_empty());
        let bnd = std::mem::take(bnd).unwrap();
        let arg = std::mem::take(arg.as_mut());
        let rules = std::mem::take(rules);
        *self = encode_match(bnd, arg, rules, ctrs, adts, adt_encoding);
      } else if let Term::Swt { arg, bnd, with, pred, arms: rules } = self {
        assert!(with.is_empty());
        let bnd = std::mem::take(bnd).unwrap();
        let arg = std::mem::take(arg.as_mut());
        let pred = std::mem::take(pred);
        let rules = std::mem::take(rules);
        *self = encode_switch(bnd, arg, pred, rules);
      }
    })
  }
}

fn encode_match(
  bnd: Name,
  arg: Term,
  mut rules: Vec<MatchRule>,
  ctrs: &Constructors,
  adts: &Adts,
  adt_encoding: AdtEncoding,
) -> Term {
  let adt_nam = ctrs.get(rules[0].0.as_ref().unwrap()).unwrap();

  // Add a `use` term reconstructing the matched variable to each arm.
  for rule in rules.iter_mut() {
    let ctr = rule.0.clone().unwrap();
    let fields = adts[adt_nam].ctrs[&ctr].iter().map(|f| Term::Var { nam: Name::new(format!("{bnd}.{f}")) });
    let orig = Term::call(Term::Ref { nam: ctr }, fields);
    rule.2 =
      Term::Use { nam: Some(bnd.clone()), val: Box::new(orig), nxt: Box::new(std::mem::take(&mut rule.2)) };
  }

  // ADT Encoding depends on compiler option
  match adt_encoding {
    // (x @field1 @field2 ... body1  @field1 body2 ...)
    AdtEncoding::Scott => {
      let mut arms = vec![];
      for rule in rules {
        let body = rule.1.iter().cloned().rfold(rule.2, |bod, nam| Term::lam(nam, bod));
        arms.push(body);
      }
      Term::call(arg, arms)
    }
    // #adt_name(x arm[0] arm[1] ...)
    AdtEncoding::TaggedScott => {
      let mut arms = vec![];
      for rule in rules.into_iter() {
        let body =
          rule.1.iter().cloned().rfold(rule.2, |bod, nam| Term::tagged_lam(Tag::adt_name(adt_nam), nam, bod));
        arms.push(body);
      }
      Term::tagged_call(Tag::adt_name(adt_nam), arg, arms)
    }
  }
}

/// Convert into a sequence of native switches, decrementing by 1 each switch.
/// switch n {0: A; 1: B; _: (C n-2)} converted to
/// switch n {0: A; _: @%x match %x {0: B; _: @n-2 (C n-2)}}
fn encode_switch(bnd: Name, arg: Term, pred: Option<Name>, mut rules: Vec<Term>) -> Term {
  // Add a `use` term reconstructing the matched variable to each arm.
  let n_nums = rules.len() - 1;
  for (i, rule) in rules.iter_mut().enumerate() {
    let orig = if i != n_nums {
      Term::Num { val: i as u64 }
    } else {
      Term::add_num(Term::Var { nam: pred.clone().unwrap() }, n_nums as u64)
    };
    *rule = Term::Use { nam: Some(bnd.clone()), val: Box::new(orig), nxt: Box::new(std::mem::take(rule)) };
  }

  // Create the cascade of switches
  let match_var = Name::new("%x");
  let (succ, nums) = rules.split_last_mut().unwrap();
  let last_arm = Term::lam(pred, std::mem::take(succ));
  nums.iter_mut().enumerate().rfold(last_arm, |term, (i, rule)| {
    let arms = vec![std::mem::take(rule), term];
    if i == 0 {
      Term::Swt { arg: Box::new(arg.clone()), bnd: Some(bnd.clone()), with: vec![], pred: None, arms }
    } else {
      let swt = Term::Swt {
        arg: Box::new(Term::Var { nam: match_var.clone() }),
        bnd: Some(match_var.clone()),
        with: vec![],
        pred: None,
        arms,
      };
      Term::named_lam(match_var.clone(), swt)
    }
  })
}
