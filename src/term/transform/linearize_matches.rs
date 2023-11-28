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
        if let Some(name) = nam {
          bod.linearize_matches_by(name);
        }
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

  pub fn linearize_matches_by(&mut self, name: &Name) {
    match self {
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.linearize_matches_by(name),

      Term::Let { pat: LetPat::Var(nam), val, nxt } => {
        val.linearize_matches_by(name);
        if name != nam {
          nxt.linearize_matches_by(name);
        }
      }

      Term::Let { pat: LetPat::Tup(fst, snd), val, nxt } | Term::Dup { fst, snd, val, nxt, .. } => {
        val.linearize_matches_by(name);
        if !(fst.as_ref().is_some_and(|n| n == name) || snd.as_ref().is_some_and(|n| n == name)) {
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
        let Term::Var { nam } = scrutinee.as_ref() else { unreachable!() };

        if nam == name || !self.free_vars().contains_key(name) {
          return;
        }

        let Term::Match { scrutinee, arms } = std::mem::take(self) else { unreachable!() };

        let arms = arms
          .into_iter()
          .map(|(rule, arm)| {
            let arm = Term::Lam { nam: Some(name.clone()), bod: Box::new(arm) };
            (rule, arm)
          })
          .collect();

        *self = Term::App {
          fun: Box::new(Term::Match { scrutinee, arms }),
          arg: Box::new(Term::Var { nam: name.clone() }),
        };
      }

      Term::Var { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Lnk { .. } | Term::Era => {}
    }
  }
}
