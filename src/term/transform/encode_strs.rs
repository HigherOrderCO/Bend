use hvmc::run::Val;
use indexmap::IndexMap;

use crate::term::{builtin_adt, Book, Name, Tag, Term};

pub const STRING: &str = "String";
pub const SNIL: &str = "SNil";
pub const SCONS: &str = "SCons";
pub const HEAD: &str = "head";
pub const TAIL: &str = "tail";

pub struct BuiltinString;

impl builtin_adt::BuiltinAdt for BuiltinString {
  fn encode_terms(&self, book: &mut Book) -> bool {
    let mut found_str = false;
    for def in book.defs.values_mut() {
      for rule in &mut def.rules {
        found_str |= rule.body.encode_str();
      }
    }
    found_str
  }

  fn constructors(&self) -> IndexMap<Name, Vec<Name>> {
    IndexMap::from([(Name::new(SCONS), vec![Name::new(HEAD), Name::new(TAIL)]), (Name::new(SNIL), vec![])])
  }

  fn name(&self) -> Name {
    Name::new(STRING)
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
      Term::Match { arms, scrutinee } => {
        let mut used = scrutinee.encode_str();
        for (pat, arm) in arms {
          used |= pat.names().chain(pat.ctrs()).any(|Name(n)| matches!(n.as_str(), SCONS | SNIL));
          used |= arm.encode_str();
        }
        used
      }
      Term::List { els } => {
        let mut used = false;
        for el in els {
          used |= el.encode_str();
        }
        used
      }
      Term::Var { nam: Name(nam) } => nam == SCONS || nam == SNIL,
      Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => false,
    }
  }
}
