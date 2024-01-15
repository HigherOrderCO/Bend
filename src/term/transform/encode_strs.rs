use hvmc::run::Val;
use indexmap::IndexMap;

use crate::term::{Adt, Book, Name, Tag, Term};

const STRING: &str = "String";
const SNIL: &str = "SNil";
const SCONS: &str = "SCons";
const HEAD: &str = "head";
const TAIL: &str = "tail";

impl Book {
  pub fn encode_strs(&mut self) -> Result<(), String> {
    if self.adts.contains_key(&Name::new(STRING)) {
      return Err("String is a built-in data type and should not be overridden.".to_string());
    }

    self.ctrs.insert(Name::new(SNIL), Name::new(STRING));
    self.ctrs.insert(Name::new(SCONS), Name::new(STRING));

    self.adts.insert(Name::new(STRING), Adt {
      ctrs: IndexMap::from([
        (Name::new(SCONS), vec![Name::new(HEAD), Name::new(TAIL)]),
        (Name::new(SNIL), vec![]),
      ]),
    });

    let mut found_str = false;
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        found_str |= rule.body.encode_str();
      }
    }

    if !found_str {
      self.ctrs.remove(&Name::new(SNIL));
      self.ctrs.remove(&Name::new(SCONS));
      self.adts.remove(&Name::new(STRING));
    }

    Ok(())
  }
}

impl Term {
  fn encode_str(&mut self) -> bool {
    match self {
      Term::Str { val } => {
        let chars = val.chars();

        let snil = Term::Var { nam: Name::new(SNIL) };
        *self = chars.rfold(snil, |acc, char| {
          let char = Term::Num { val: Val::from(char) };
          let scons_app = Term::App {
            tag: Tag::Static,
            fun: Box::new(Term::Var { nam: Name::new(SCONS) }),
            arg: Box::new(char),
          };
          Term::App { tag: Tag::Static, fun: Box::new(scons_app), arg: Box::new(acc) }
        });
        true
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_str(),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        let fst_uses = fst.encode_str();
        let snd_uses = snd.encode_str();
        fst_uses || snd_uses
      }
      Term::Match { arms, .. } => {
        let mut used = false;
        for arm in arms {
          used |= arm.1.encode_str();
        }
        used
      }
      Term::Var { nam: Name(nam) } => nam == SCONS || nam == SNIL,
      Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => false,
    }
  }
}
