use std::collections::BTreeMap;

use crate::term::{Adt, Book, Name, RulePat, Term};

impl Book {
  pub fn simplify_matches(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.simplify_matches(&self.adts);
      }
    }
  }
}

#[allow(unused_variables)]
impl Term {
  pub fn simplify_matches(&mut self, adts: &BTreeMap<Name, Adt>) {
    fn go(term: &mut Term, adts: &BTreeMap<Name, Adt>) {
      match term {
        Term::Match { scrutinee, arms } => {
          if matches!(arms[0], (RulePat::Num(_), _)) {
            go(scrutinee, adts);

            for (_, term) in arms {
              go(term, adts);
            }
          } else {
            let Term::Match { scrutinee, arms } = std::mem::replace(term, Term::Era) else { unreachable!() };

            let Term::Var { nam } = *scrutinee else {
              unreachable!(); // the scrutinee of a match on adts should always be a var
            };

            *term = Term::make_match_app(nam, arms, adts);
          }
        }

        Term::Lam { bod, .. } | Term::Chn { bod, .. } => go(bod, adts),

        Term::App { fun: fst, arg: snd }
        | Term::Let { val: fst, nxt: snd, .. }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Tup { fst, snd }
        | Term::Sup { fst, snd }
        | Term::Opx { fst, snd, .. } => {
          go(fst, adts);
          go(snd, adts);
        }

        Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
      }
    }

    go(self, adts)
  }

  #[allow(unused_mut)]
  fn make_match_app(nam: Name, arms: Vec<(RulePat, Term)>, adts: &BTreeMap<Name, Adt>) -> Self {
    let mut res = Term::Var { nam };
    
    for (rule, arm) in arms {
      todo!("[{:?} => {:?}]", rule, arm)
    }

    res
  }
}
