use indexmap::IndexMap;

use crate::term::{builtin_adt, Book, Name, Pattern, Term};

pub const LIST: &str = "List";
pub const LCONS: &str = "LCons";
pub const LNIL: &str = "LNil";
pub const HEAD: &str = "head";
pub const TAIL: &str = "tail";

pub struct BuiltinList;

impl builtin_adt::BuiltinAdt for BuiltinList {
  fn constructors(&self) -> IndexMap<Name, Vec<Name>> {
    IndexMap::from([(Name::new(LCONS), vec![Name::new(HEAD), Name::new(TAIL)]), (Name::new(LNIL), vec![])])
  }

  fn name(&self) -> Name {
    Name::new(LIST)
  }

  fn encode_terms(&self, book: &mut Book) -> bool {
    let mut found_list = false;
    for def in book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.pats.iter_mut().for_each(|pat| found_list |= pat.encode_lists());
        found_list |= rule.body.encode_lists();
      }
    }
    found_list
  }
}

impl Term {
  pub fn encode_lists(&mut self) -> bool {
    match self {
      Term::List { els } => {
        let lnil = Term::Var { nam: Name::new(LNIL) };

        *self = els.iter_mut().rfold(lnil, |acc, nxt| {
          nxt.encode_lists();
          let lcons = Term::Var { nam: Name::new(LCONS) };
          Term::call(lcons, [nxt.clone(), acc])
        });

        true
      }
      Term::Let { pat, val, nxt } => {
        let pat_uses = pat.encode_lists();
        let val_uses = val.encode_lists();
        let nxt_uses = nxt.encode_lists();
        pat_uses || val_uses || nxt_uses
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.encode_lists(),
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. } => {
        let fst_uses = fst.encode_lists();
        let snd_uses = snd.encode_lists();
        fst_uses || snd_uses
      }
      Term::Match { scrutinee, arms } => {
        let mut used = scrutinee.encode_lists();
        for (pat, arm) in arms {
          used |= pat.names().chain(pat.ctrs()).any(|Name(n)| matches!(n.as_str(), LCONS | LNIL));
          used |= arm.encode_lists();
        }
        used
      }
      Term::Var { nam: Name(nam) } => nam == LCONS || nam == LNIL,
      Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => false,
    }
  }
}

impl Pattern {
  pub fn encode_lists(&mut self) -> bool {
    match self {
      Pattern::List(pats) => {
        let lnil = Pattern::Var(Some(Name::new(LNIL)));

        *self = pats.iter_mut().rfold(lnil, |acc, nxt| {
          nxt.encode_lists();
          Pattern::Ctr(Name::new(LCONS), vec![nxt.clone(), acc])
        });

        true
      }
      Pattern::Ctr(_, pats) => {
        let mut uses = false;
        for pat in pats {
          uses |= pat.encode_lists();
        }
        uses
      }
      Pattern::Tup(fst, snd) => {
        let fst_uses = fst.encode_lists();
        let snd_uses = snd.encode_lists();
        fst_uses || snd_uses
      }
      Pattern::Var(..) | Pattern::Num(..) => false,
    }
  }
}
