use indexmap::IndexMap;

use crate::term::{Adt, Book, Name, Tag, Term};

const LIST: &str = "List";
const LCONS: &str = "LCons";
const LNIL: &str = "LNil";
const HEAD: &str = "head";
const TAIL: &str = "tail";

impl Book {
  pub fn encode_lists(&mut self) -> Result<(), String> {
    if self.adts.contains_key(&Name::new(LIST)) {
      return Err("List is a built-in data type and should not be overridden.".to_string());
    }

    self.declare_list_adt();

    let mut found_list = false;
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        found_list |= rule.body.encode_lists();
      }
    }

    if !found_list {
      self.ctrs.remove(&Name::new(LNIL));
      self.ctrs.remove(&Name::new(LCONS));
      self.adts.remove(&Name::new(LIST));
    }

    Ok(())
  }

  fn declare_list_adt(&mut self) {
    self.ctrs.insert(Name::new(LNIL), Name::new(LIST));
    self.ctrs.insert(Name::new(LCONS), Name::new(LIST));

    self.adts.insert(Name::new(LIST), Adt {
      ctrs: IndexMap::from([
        (Name::new(LCONS), vec![Name::new(HEAD), Name::new(TAIL)]),
        (Name::new(LNIL), vec![]),
      ]),
    });
  }
}

impl Term {
  pub fn encode_lists(&mut self) -> bool {
    match self {
      Term::List { els } => {
        let lnil = Term::Var { nam: Name::new(LNIL) };

        *self = els.into_iter().rfold(lnil, |acc, nxt| {
          nxt.encode_lists();
          let lcons_app = Term::App {
            tag: Tag::Static,
            fun: Box::new(Term::Var { nam: Name::new(LCONS) }),
            arg: Box::new(nxt.clone()),
          };
          Term::App { tag: Tag::Static, fun: Box::new(lcons_app), arg: Box::new(acc) }
        });

        true
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_lists(),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. } => {
        let fst_uses = fst.encode_lists();
        let snd_uses = snd.encode_lists();
        fst_uses || snd_uses
      }
      Term::Match { scrutinee, arms } => {
        let mut used = false;
        let scrutinee_used = scrutinee.encode_lists();
        for arm in arms {
          used |= arm.1.encode_lists();
        }
        scrutinee_used || used
      }
      Term::Lnk { .. }
      | Term::Ref { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Var { .. }
      | Term::Era => false,
    }
  }
}
