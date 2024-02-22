use std::fmt::Display;

use itertools::Itertools;

use crate::{
  diagnostics::ERR_INDENT_SIZE,
  term::{
    check::type_check::infer_type, display::DisplayFn, AdtEncoding, Adts, Book, Constructors, MatchNum, Name,
    Pattern, Rule, Tag, Term, Type,
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
  let typ = infer_type(rules.iter().map(|r| &r.pats[0]), ctrs).unwrap();
  match typ {
    // Just move the match into a let term
    Type::Any | Type::Tup => {
      let rule = rules.into_iter().next().unwrap();
      Term::Let { pat: rule.pats.into_iter().next().unwrap(), val: Box::new(arg), nxt: Box::new(rule.body) }
    }
    // Detach the variable from the match, converting into a native num match
    Type::Num => {
      let mut rules = rules;
      let Pattern::Num(MatchNum::Succ(Some(var))) =
        std::mem::replace(&mut rules[1].pats[0], Pattern::Num(MatchNum::Succ(None)))
      else {
        unreachable!()
      };
      rules[1].body = Term::lam(var, std::mem::take(&mut rules[1].body));
      Term::Mat { args: vec![arg], rules }
    }
    // ADT Encoding depends on compiler option
    Type::Adt(adt) => {
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
