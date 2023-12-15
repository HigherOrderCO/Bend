use hvmc::run::Val;

use crate::term::{Book, Name, Tag, Term};

impl Book {
  pub fn encode_strs(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.encode_str();
      }
    }
  }
}

impl Term {
  fn encode_str(&mut self) {
    match self {
      Term::Str { val } => {
        let mut len = 0;
        let chars = val.chars();

        let term = Term::Var { nam: Name::new("x") };
        let term = chars.rfold(term, |acc, char| {
          len += 1;
          let char = Term::Num { val: Val::from(char) };
          Term::Tup { fst: Box::new(char), snd: Box::new(acc) }
        });
        let term = Term::Lam { tag: Tag::str(), nam: Some(Name::new("x")), bod: Box::new(term) };

        let len = Term::Num { val: len as Val };

        *self = Term::Tup { fst: Box::new(len), snd: Box::new(term) };
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.encode_str();
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.encode_str();
        snd.encode_str();
      }
      Term::Match { arms, .. } => {
        for arm in arms {
          arm.1.encode_str();
        }
      }
      Term::Lnk { .. } | Term::Num { .. } | Term::Var { .. } | Term::Ref { .. } | Term::Era => {}
    }
  }
}
