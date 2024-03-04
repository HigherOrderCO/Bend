use super::{parser::parse_book, Book, Name, NumCtr, Pattern, Term};

const BUILTINS: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/term/builtins.hvm"));

pub const LIST: &str = "List";
pub const LCONS: &str = "List.cons";
pub const LNIL: &str = "List.nil";

pub const HEAD: &str = "head";
pub const TAIL: &str = "tail";

pub const STRING: &str = "String";
pub const SCONS: &str = "String.cons";
pub const SNIL: &str = "String.nil";

impl Book {
  pub fn builtins() -> Book {
    parse_book(BUILTINS, Book::default, true).expect("Error parsing builtin file, this should not happen")
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
    match self {
      Term::Lst { els } => *self = Term::encode_list(std::mem::take(els)),
      Term::Str { val } => *self = Term::encode_str(val),
      Term::Let { pat, val, nxt } => {
        pat.encode_builtins();
        val.encode_builtins();
        nxt.encode_builtins();
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_builtins(),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. } => {
        fst.encode_builtins();
        snd.encode_builtins();
      }
      Term::Sup { els, .. } | Term::Tup { els } => {
        for el in els {
          el.encode_builtins();
        }
      }
      Term::Mat { args, rules } => {
        for arg in args {
          arg.encode_builtins();
        }
        for rule in rules {
          for pat in &mut rule.pats {
            pat.encode_builtins();
          }
          rule.body.encode_builtins();
        }
      }
      Term::Var { .. } | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Era | Term::Err => {}
    }
  }

  fn encode_list(elements: Vec<Term>) -> Term {
    elements.into_iter().rfold(Term::r#ref(LNIL), |acc, mut nxt| {
      nxt.encode_builtins();
      Term::call(Term::r#ref(LCONS), [nxt, acc])
    })
  }

  fn encode_str(val: &str) -> Term {
    val.chars().rfold(Term::r#ref(SNIL), |acc, char| {
      Term::call(Term::r#ref(SCONS), [Term::Num { val: u64::from(char) }, acc])
    })
  }
}

impl Pattern {
  pub fn encode_builtins(&mut self) {
    match self {
      Pattern::Lst(pats) => *self = Self::encode_list(std::mem::take(pats)),
      Pattern::Str(str) => *self = Self::encode_str(str),
      Pattern::Ctr(_, pats) | Pattern::Tup(pats) => {
        for pat in pats {
          pat.encode_builtins();
        }
      }
      Pattern::Var(..) | Pattern::Num(..) => {}
    }
  }

  fn encode_list(elements: Vec<Pattern>) -> Pattern {
    let lnil = Pattern::Ctr(Name::from(LNIL), vec![]);

    elements.into_iter().rfold(lnil, |acc, mut nxt| {
      nxt.encode_builtins();
      Pattern::Ctr(Name::from(LCONS), vec![nxt, acc])
    })
  }

  fn encode_str(str: &str) -> Pattern {
    let lnil = Pattern::Ctr(Name::from(SNIL), vec![]);

    str.chars().rfold(lnil, |tail, head| {
      let head = Pattern::Num(NumCtr::Num(head as u64));
      Pattern::Ctr(Name::from(SCONS), vec![head, tail])
    })
  }
}
