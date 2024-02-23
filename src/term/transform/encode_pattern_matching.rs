use std::fmt::Display;

use itertools::Itertools;

use crate::{
  diagnostics::ERR_INDENT_SIZE,
  term::{
    check::type_check::infer_match_arg_type, display::DisplayFn, AdtEncoding, Adts, Book, Constructors, Name,
    NumCtr, Pattern, Rule, Tag, Term, Type,
  },
};

impl Book {
  /// Encodes pattern matching expressions in the book functions according to the adt_encoding.
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
    match self {
      Term::Mat { .. } => {
        debug_assert!(self.is_simple_match(ctrs, adts), "{self}");
        let Term::Mat { args, rules } = self else { unreachable!() };
        for rule in rules.iter_mut() {
          rule.body.encode_simple_matches(ctrs, adts, adt_encoding);
        }
        let arg = std::mem::take(&mut args[0]);
        let rules = std::mem::take(rules);
        *self = encode_match(arg, rules, ctrs, adts, adt_encoding);
      }
      Term::Lst { els } => els.iter_mut().for_each(|e| e.encode_simple_matches(ctrs, adts, adt_encoding)),
      Term::Let { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.encode_simple_matches(ctrs, adts, adt_encoding);
        snd.encode_simple_matches(ctrs, adts, adt_encoding);
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_simple_matches(ctrs, adts, adt_encoding),
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
  }
}

fn encode_match(
  arg: Term,
  rules: Vec<Rule>,
  ctrs: &Constructors,
  adts: &Adts,
  adt_encoding: AdtEncoding,
) -> Term {
  let typ = infer_match_arg_type(&rules, 0, ctrs).unwrap();
  match typ {
    Type::Any | Type::Tup => {
      let fst_rule = rules.into_iter().next().unwrap();
      encode_var(arg, fst_rule)
    }
    Type::NumSucc(_) => encode_num_succ(arg, rules),
    Type::Num => encode_num(arg, rules),
    // ADT Encoding depends on compiler option
    Type::Adt(adt) => encode_adt(arg, rules, adt, adt_encoding, adts),
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

  rules.into_iter().rev().fold(last_arm, |term, rule| {
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

fn encode_adt(arg: Term, rules: Vec<Rule>, adt: Name, adt_encoding: AdtEncoding, adts: &Adts) -> Term {
  match adt_encoding {
    // (x @field1 @field2 ... body1  @field1 body2 ...)
    AdtEncoding::Scott => {
      let mut arms = vec![];
      for rule in rules {
        let body = rule.body;
        let body = rule.pats[0].binds().rev().fold(body, |bod, nam| Term::named_lam(nam.clone(), bod));
        arms.push(body);
      }
      Term::call(arg, arms)
    }
    // #adt_name(x arm[0] arm[1] ...)
    AdtEncoding::TaggedScott => {
      let mut arms = vec![];
      for (rule, (ctr, fields)) in rules.into_iter().zip(&adts[&adt].ctrs) {
        let body =
          rule.pats[0].binds().rev().zip(fields.iter().rev()).fold(rule.body, |bod, (var, field)| {
            Term::tagged_lam(Tag::adt_field(&adt, ctr, field), var.clone(), bod)
          });
        arms.push(body);
      }
      Term::tagged_call(Tag::adt_name(&adt), arg, arms)
    }
  }
}

#[derive(Debug, Clone)]
pub enum MatchErr {
  RepeatedBind(Name),
  LetPat(Box<MatchErr>),
  Linearize(Name),
  NotExhaustive(ExhaustivenessErr),
  TypeMismatch(Type, Type),
  ArityMismatch(usize, usize),
  CtrArityMismatch(Name, usize, usize),
  MalformedNumSucc(Pattern, Pattern),
}

#[derive(Debug, Clone)]
pub struct ExhaustivenessErr(pub Vec<Name>);

const PATTERN_ERROR_LIMIT: usize = 5;
const ERROR_LIMIT_HINT: &str = "Use the --verbose option to see all cases.";

impl MatchErr {
  pub fn display(&self, verbose: bool) -> impl std::fmt::Display + '_ {
    DisplayFn(move |f| match self {
      MatchErr::RepeatedBind(bind) => write!(f, "Repeated var name in a match block: {}", bind),
      MatchErr::LetPat(err) => {
        let let_err = err.to_string().replace("match block", "let bind");
        write!(f, "{let_err}")?;

        if matches!(err.as_ref(), MatchErr::NotExhaustive(..)) {
          write!(f, "\nConsider using a match block instead")?;
        }

        Ok(())
      }
      MatchErr::Linearize(var) => write!(f, "Unable to linearize variable {var} in a match block."),
      MatchErr::NotExhaustive(err) if verbose => write!(f, "{}", err.display_with_limit(usize::MAX)),
      MatchErr::NotExhaustive(err) => write!(f, "{err}"),
      MatchErr::TypeMismatch(got, exp) => {
        write!(f, "Type mismatch in pattern matching. Expected '{exp}', found '{got}'.")
      }
      MatchErr::ArityMismatch(got, exp) => {
        write!(f, "Arity mismatch in pattern matching. Expected {exp} patterns, found {got}.")
      }
      MatchErr::CtrArityMismatch(ctr, got, exp) => write!(
        f,
        "Constructor arity mismatch in pattern matching. Constructor '{ctr}' expects {exp} fields, found {got}."
      ),
      MatchErr::MalformedNumSucc(got, exp) => {
        write!(f, "Expected a sequence of incrementing numbers ending with '{exp}', found '{got}'.")
      }
    })
  }
}

impl std::fmt::Display for MatchErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display(false))
  }
}

impl ExhaustivenessErr {
  pub fn display_with_limit(&self, limit: usize) -> String {
    let ident = ERR_INDENT_SIZE * 2;
    let hints =
      self.0.iter().take(limit).map(|pat| format!("{:ident$}Case '{pat}' not covered.", "")).join("\n");

    let mut str = format!("Non-exhaustive pattern matching. Hint:\n{}", hints);

    let len = self.0.len();
    if len > limit {
      str.push_str(&format!(" ... and {} others.\n{:ident$}{}", len - limit, "", ERROR_LIMIT_HINT))
    }

    str
  }
}

impl Display for ExhaustivenessErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display_with_limit(PATTERN_ERROR_LIMIT))
  }
}
