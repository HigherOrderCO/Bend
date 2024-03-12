use crate::term::{
  check::type_check::infer_match_arg_type, AdtEncoding, Adts, Book, Constructors, Name, NumCtr, Pattern,
  Rule, Tag, Term, Type,
};

impl Book {
  /// Encodes simple pattern matching expressions in the book into
  /// their native/core form.
  /// See [`super::simplify_matches::simplify_match_expression`] for
  /// the meaning of "simple" used here.
  ///
  /// ADT matches are encoded based on `adt_encoding`.
  ///
  /// Num matches are encoded as a sequence of native num matches (on 0 and 1+).
  ///
  /// Var and pair matches become a let expression.
  pub fn encode_simple_matches(&mut self, adt_encoding: AdtEncoding) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.encode_simple_matches(&self.ctrs, &self.adts, adt_encoding);
      }
    }
  }
}

impl Term {
  pub fn encode_simple_matches(&mut self, ctrs: &Constructors, adts: &Adts, adt_encoding: AdtEncoding) {
    Term::recursive_call(move || {
      for child in self.children_mut() {
        child.encode_simple_matches(ctrs, adts, adt_encoding)
      }

      if let Term::Mat { .. } = self {
        debug_assert!(self.is_simple_match(ctrs, adts), "{self}");
        let Term::Mat { args, rules } = self else { unreachable!() };
        let arg = std::mem::take(&mut args[0]);
        let rules = std::mem::take(rules);
        *self = encode_match(arg, rules, ctrs, adt_encoding);
      }
    })
  }
}

fn encode_match(arg: Term, rules: Vec<Rule>, ctrs: &Constructors, adt_encoding: AdtEncoding) -> Term {
  let typ = infer_match_arg_type(&rules, 0, ctrs).unwrap();
  match typ {
    Type::Any | Type::Tup(_) => {
      let fst_rule = rules.into_iter().next().unwrap();
      encode_var(arg, fst_rule)
    }
    Type::NumSucc(_) => encode_num_succ(arg, rules),
    Type::Num => encode_num(arg, rules),
    // ADT Encoding depends on compiler option
    Type::Adt(adt) => encode_adt(arg, rules, adt, adt_encoding),
  }
}

/// Just move the match into a let term
fn encode_var(arg: Term, rule: Rule) -> Term {
  Term::Let { pat: rule.pats.into_iter().next().unwrap(), val: Box::new(arg), nxt: Box::new(rule.body) }
}

/// Convert into a sequence of native matches, decrementing by 1 each match.
/// match n {0: A; 1: B; 2+: (C n-2)} converted to
/// match n {0: A; 1+: @%x match %x {0: B; 1+: @n-2 (C n-2)}}
fn encode_num_succ(arg: Term, mut rules: Vec<Rule>) -> Term {
  let last_rule = rules.pop().unwrap();

  let match_var = Name::from("%x");

  // @n-2 (C n-2)
  let Pattern::Num(NumCtr::Succ(_, Some(last_var))) = last_rule.pats.into_iter().next().unwrap() else {
    unreachable!()
  };
  let last_arm = Term::lam(last_var, last_rule.body);

  rules.into_iter().rfold(last_arm, |term, rule| {
    let rules = vec![Rule { pats: vec![Pattern::Num(NumCtr::Num(0))], body: rule.body }, Rule {
      pats: vec![Pattern::Num(NumCtr::Succ(1, None))],
      body: term,
    }];

    let Pattern::Num(NumCtr::Num(num)) = rule.pats.into_iter().next().unwrap() else { unreachable!() };
    if num == 0 {
      Term::Mat { args: vec![arg.clone()], rules }
    } else {
      let mat = Term::Mat { args: vec![Term::Var { nam: match_var.clone() }], rules };
      Term::named_lam(match_var.clone(), mat)
    }
  })
}

/// Convert into a sequence of native matches on (- n num_case)
/// match n {a: A; b: B; n: (C n)} converted to
/// match (- n a) {0: A; 1+: @%p match (- %p b-a-1) { 0: B; 1+: @%p let n = (+ %p b+1); (C n)}}
fn encode_num(arg: Term, mut rules: Vec<Rule>) -> Term {
  fn go(
    mut rules: impl Iterator<Item = Rule>,
    last_rule: Rule,
    prev_num: Option<u64>,
    arg: Option<Term>,
  ) -> Term {
    let rule = rules.next();
    match (prev_num, rule) {
      // Num matches must have at least 2 rules (1 num and 1 var).
      // The first rule can't also be the last.
      (None, None) => unreachable!(),
      // First match
      // match (- n a) {0: A; 1+: ...}
      (None, Some(rule)) => {
        let Pattern::Num(NumCtr::Num(val)) = &rule.pats[0] else { unreachable!() };
        Term::native_num_match(
          Term::sub_num(arg.unwrap(), *val),
          rule.body,
          go(rules, last_rule, Some(*val), None),
          None,
        )
      }
      // Middle match
      // @%p match (- %p b-a-1) { 0: B; 1+: ... }
      (Some(prev_num), Some(rule)) => {
        let Pattern::Num(NumCtr::Num(val)) = &rule.pats[0] else { unreachable!() };
        let pred_nam = Name::from("%p");
        Term::named_lam(
          pred_nam.clone(),
          Term::native_num_match(
            Term::sub_num(Term::Var { nam: pred_nam }, val.wrapping_sub(prev_num).wrapping_sub(1)),
            rule.body,
            go(rules, last_rule, Some(*val), None),
            None,
          ),
        )
      }
      // Last match
      // @%p let n = (+ %p b+1); (C n)
      (Some(prev_num), None) => {
        let pred_nam = Name::from("%p");
        Term::named_lam(pred_nam.clone(), Term::Let {
          pat: last_rule.pats.into_iter().next().unwrap(),
          val: Box::new(Term::add_num(Term::Var { nam: pred_nam }, prev_num.wrapping_add(1))),
          nxt: Box::new(last_rule.body),
        })
      }
    }
  }

  let last_rule = rules.pop().unwrap();
  go(rules.into_iter(), last_rule, None, Some(arg))
}

fn encode_adt(arg: Term, rules: Vec<Rule>, adt: Name, adt_encoding: AdtEncoding) -> Term {
  match adt_encoding {
    // (x @field1 @field2 ... body1  @field1 body2 ...)
    AdtEncoding::Scott => {
      let mut arms = vec![];
      for rule in rules {
        let body = rule.pats[0].binds().cloned().rfold(rule.body, |bod, nam| Term::lam(nam, bod));
        arms.push(body);
      }
      Term::call(arg, arms)
    }
    // #adt_name(x arm[0] arm[1] ...)
    AdtEncoding::TaggedScott => {
      let mut arms = vec![];
      for rule in rules.into_iter() {
        let body = rule.pats[0]
          .binds()
          .cloned()
          .rfold(rule.body, |bod, nam| Term::tagged_lam(Tag::adt_name(&adt), nam, bod));
        arms.push(body);
      }
      Term::tagged_call(Tag::adt_name(&adt), arg, arms)
    }
  }
}
