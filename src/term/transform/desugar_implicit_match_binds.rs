use crate::term::{Adt, Book, MatchNum, Name, Pattern, Term};
use std::collections::{BTreeMap, HashMap};

impl Book {
  pub fn desugar_implicit_match_binds(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.desugar_implicit_match_binds(&self.ctrs, &self.adts);
      }
    }
  }
}

impl Term {
  pub fn desugar_implicit_match_binds(&mut self, ctrs: &HashMap<Name, Name>, adts: &BTreeMap<Name, Adt>) {
    match self {
      Term::Match { scrutinee, arms } => {
        let Term::Var { nam: scrutinee } = scrutinee.as_ref() else { unreachable!() };
        for (pat, body) in arms {
          match pat {
            Pattern::Var(_) => (),
            Pattern::Ctr(nam, pat_args) => {
              let adt = &ctrs[nam];
              let Adt { ctrs } = &adts[adt];
              let ctr_args = &ctrs[nam.as_ref()];
              if pat_args.is_empty() && !ctr_args.is_empty() {
                // Implicit ctr args
                *pat_args =
                  ctr_args.iter().map(|x| Pattern::Var(Some(Name(format!("{scrutinee}.{x}"))))).collect();
              }
            }
            Pattern::Num(MatchNum::Zero) => (),
            Pattern::Num(MatchNum::Succ(Some(_))) => (),
            Pattern::Num(MatchNum::Succ(p @ None)) => {
              // Implicit num arg
              *p = Some(Some(Name(format!("{scrutinee}-1"))));
            }
            Pattern::Tup(_, _) => (),
          }
          body.desugar_implicit_match_binds(ctrs, adts);
        }
      }
      Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.desugar_implicit_match_binds(ctrs, adts);
        snd.desugar_implicit_match_binds(ctrs, adts);
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.desugar_implicit_match_binds(ctrs, adts);
      }
      Term::Era | Term::Ref { .. } | Term::Num { .. } | Term::Lnk { .. } | Term::Var { .. } => (),
      Term::Let { pat: _, .. } => {
        unreachable!("Expected destructor let expressions to have been desugared already")
      }
    }
  }
}
