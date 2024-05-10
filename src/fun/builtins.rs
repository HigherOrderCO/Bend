use super::{parser::TermParser, Book, Name, Num, Pattern, Term};
use crate::maybe_grow;

const BUILTINS: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/fun/builtins.hvm"));

pub const LIST: &str = "List";
pub const LCONS: &str = "List/cons";
pub const LNIL: &str = "List/nil";

pub const HEAD: &str = "head";
pub const TAIL: &str = "tail";

pub const STRING: &str = "String";
pub const SCONS: &str = "String/cons";
pub const SNIL: &str = "String/nil";

pub const RESULT: &str = "Result";
pub const RESULT_OK: &str = "Result/ok";
pub const RESULT_ERR: &str = "Result/err";

pub const NAT: &str = "Nat";
pub const NAT_SUCC: &str = "Nat/succ";
pub const NAT_ZERO: &str = "Nat/zero";

impl Book {
  pub fn builtins() -> Book {
    TermParser::new(BUILTINS)
      .parse_book(Book::default(), true)
      .expect("Error parsing builtin file, this should not happen")
  }

  pub fn encode_builtins(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.pats.iter_mut().for_each(Pattern::encode_builtins);
        rule.body.encode_builtins();
      }
    }
  }
}

impl Term {
  fn encode_builtins(&mut self) {
    maybe_grow(|| match self {
      Term::List { els } => *self = Term::encode_list(std::mem::take(els)),
      Term::Str { val } => *self = Term::encode_str(val),
      Term::Nat { val } => *self = Term::encode_nat(*val),
      _ => {
        for child in self.children_mut() {
          child.encode_builtins();
        }
      }
    })
  }

  fn encode_list(elements: Vec<Term>) -> Term {
    elements.into_iter().rfold(Term::r#ref(LNIL), |acc, mut nxt| {
      nxt.encode_builtins();
      Term::call(Term::r#ref(LCONS), [nxt, acc])
    })
  }

  pub fn encode_str(val: &str) -> Term {
    val.chars().rfold(Term::r#ref(SNIL), |acc, char| {
      Term::call(Term::r#ref(SCONS), [Term::Num { val: Num::U24(char as u32 & 0x00ff_ffff) }, acc])
    })
  }

  pub fn encode_nat(val: u32) -> Term {
    (0 .. val).fold(Term::r#ref(NAT_ZERO), |acc, _| Term::app(Term::r#ref(NAT_SUCC), acc))
  }

  pub fn encode_ok(val: Term) -> Term {
    Term::call(Term::r#ref(RESULT_OK), [val])
  }

  pub fn encode_err(val: Term) -> Term {
    Term::call(Term::r#ref(RESULT_ERR), [val])
  }
}

impl Pattern {
  pub fn encode_builtins(&mut self) {
    match self {
      Pattern::Lst(pats) => *self = Self::encode_list(std::mem::take(pats)),
      Pattern::Str(str) => *self = Self::encode_str(str),
      _ => {
        for pat in self.children_mut() {
          pat.encode_builtins();
        }
      }
    }
  }

  fn encode_list(elements: Vec<Pattern>) -> Pattern {
    let lnil = Pattern::Ctr(Name::new(LNIL), vec![]);

    elements.into_iter().rfold(lnil, |acc, mut nxt| {
      nxt.encode_builtins();
      Pattern::Ctr(Name::new(LCONS), vec![nxt, acc])
    })
  }

  fn encode_str(str: &str) -> Pattern {
    let lnil = Pattern::Ctr(Name::new(SNIL), vec![]);

    str.chars().rfold(lnil, |tail, head| {
      let head = Pattern::Num(head as u32);
      Pattern::Ctr(Name::new(SCONS), vec![head, tail])
    })
  }
}
