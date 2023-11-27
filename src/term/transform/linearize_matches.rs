use crate::{
  term::{Book, LetPat, Name},
  Term,
};

impl Book {
  pub fn linearize_matches(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_matches();
      }
    }
  }
}

impl Term {
  pub fn linearize_matches(&mut self) {
    match self {
      Term::Lam { nam, bod } => {
        bod.linearize_matches();
        bod.linearize_matches_by(nam.as_ref())
      }

      Term::Chn { bod, .. } => bod.linearize_matches(),

      Term::Match { scrutinee, arms } => {
        scrutinee.linearize_matches();
        for (_, arm) in arms {
          arm.linearize_matches();
        }
      }

      Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.linearize_matches();
        snd.linearize_matches();
      }

      Term::Var { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Lnk { .. } | Term::Era => {}
    }
  }

  fn linearize_matches_by(&mut self, name: Option<&Name>) {
    match self {
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.linearize_matches_by(name),

      Term::Let { pat: LetPat::Var(nam), val, nxt } => {
        val.linearize_matches_by(name);
        if !name.is_some_and(|n| n == nam) {
          nxt.linearize_matches_by(name);
        }
      }

      Term::Let { pat: LetPat::Tup(fst, snd), val, nxt } | Term::Dup { fst, snd, val, nxt, .. } => {
        val.linearize_matches_by(name);
        if !name
          .is_some_and(|nam| fst.as_ref().is_some_and(|n| n == nam) || snd.as_ref().is_some_and(|n| n == nam))
        {
          nxt.linearize_matches_by(name);
        }
      }

      Term::App { fun: fst, arg: snd }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.linearize_matches_by(name);
        snd.linearize_matches_by(name);
      }

      Term::Match { scrutinee, .. } => {
        let Term::Var { nam } = &**scrutinee else { unreachable!() };

        if name.is_some_and(|n| n == nam) {
          return;
        }

        let Term::Match { scrutinee, arms } = std::mem::take(self) else { unreachable!() };

        let arms = arms
          .into_iter()
          .map(|(rule, arm)| {
            let arm = Term::Lam { nam: name.cloned(), bod: Box::new(arm) };
            (rule, arm)
          })
          .collect();

        *self = Term::App {
          fun: Box::new(Term::Match { scrutinee, arms }),
          arg: Box::new(name.cloned().map(|nam| Term::Var { nam }).unwrap_or_default()),
        };
      }

      Term::Var { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Lnk { .. } | Term::Era => {}
    }
  }
}
