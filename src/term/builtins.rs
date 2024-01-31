use super::{parser::parse_definition_book, Book, Name, Origin, Pattern, Term};
use hvmc::run::Val;

const BUILTINS: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/term/builtins.hvm"));

pub const LIST: &str = "List";
pub const LCONS: &str = "LCons";
pub const LNIL: &str = "LNil";

pub const HEAD: &str = "head";
pub const TAIL: &str = "tail";

pub const STRING: &str = "String";
pub const SNIL: &str = "SNil";
pub const SCONS: &str = "SCons";

impl Book {
  pub fn builtins() -> Book {
    parse_definition_book(BUILTINS, Book::new, Origin::Builtin)
      .expect("Error parsing builtin file, this should not happen")
  }

  pub fn encode_builtins(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.pats.iter_mut().for_each(|pat| pat.encode_builtins());
        rule.body.encode_builtins();
      }
    }
  }
}

impl Term {
  fn encode_builtins(&mut self) {
    match self {
      Term::List { els } => *self = Term::encode_list(std::mem::take(els)),
      Term::Str { val } => *self = Term::encode_str(std::mem::take(val)),
      Term::Let { pat, val, nxt } => {
        pat.encode_builtins();
        val.encode_builtins();
        nxt.encode_builtins();
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_builtins(),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. } => {
        fst.encode_builtins();
        snd.encode_builtins();
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.encode_builtins();
        for (pat, arm) in arms {
          pat.encode_builtins();
          arm.encode_builtins();
        }
      }
      Term::Var { .. } | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Era => {}
    }
  }

  fn encode_list(elements: Vec<Term>) -> Term {
    let lnil = Term::Var { nam: Name::new(LNIL) };

    elements.into_iter().rfold(lnil, |acc, mut nxt| {
      nxt.encode_builtins();
      Term::call(Term::Var { nam: Name::new(LCONS) }, [nxt, acc])
    })
  }

  fn encode_str(val: String) -> Term {
    let snil = Term::Var { nam: Name::new(SNIL) };

    val.chars().rfold(snil, |acc, char| {
      Term::call(Term::Var { nam: Name::new(SCONS) }, [Term::Num { val: Val::from(char) }, acc])
    })
  }
}

impl Pattern {
  pub fn encode_builtins(&mut self) {
    match self {
      Pattern::List(pats) => *self = Self::encode_list(std::mem::take(pats)),
      Pattern::Ctr(Name(_), pats) => {
        for pat in pats {
          pat.encode_builtins();
        }
      }
      Pattern::Tup(fst, snd) => {
        fst.encode_builtins();
        snd.encode_builtins();
      }
      Pattern::Var(..) | Pattern::Num(..) => {}
    }
  }

  fn encode_list(elements: Vec<Pattern>) -> Pattern {
    let lnil = Pattern::Var(Some(Name::new(LNIL)));

    elements.into_iter().rfold(lnil, |acc, mut nxt| {
      nxt.encode_builtins();
      Pattern::Ctr(Name::new(LCONS), vec![nxt, acc])
    })
  }
}
