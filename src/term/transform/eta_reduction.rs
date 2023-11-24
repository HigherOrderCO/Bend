use crate::term::*;

impl Book {
  pub fn eta_reduction(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.eta_reduction();
      }
    }
  }
}

impl Term {
  pub fn eta_reduction(&mut self) {
    match self {
      Term::Lam { nam: Some(lam_var), bod } => match &mut **bod {
        Term::App { fun, arg } => match &**arg {
          Term::Var { nam } if nam == lam_var => {
            let Term::App { fun, .. } = *std::mem::replace(bod, Box::new(Term::Era)) else { unreachable!() };
            *self = *fun;
            self.eta_reduction();
          }
          _ => {
            fun.eta_reduction();
            arg.eta_reduction();
          }
        },
        _ => bod.eta_reduction(),
      },
      Term::Lam { nam: _, bod } | Term::Chn { nam: _, bod } => bod.eta_reduction(),
      Term::Let { pat: _, val, nxt } | Term::Dup { tag: _, fst: _, snd: _, val, nxt } => {
        val.eta_reduction();
        nxt.eta_reduction();
      }
      Term::App { fun, arg } => {
        fun.eta_reduction();
        arg.eta_reduction();
      }
      Term::Tup { fst, snd } | Term::Sup { fst, snd } | Term::Opx { op: _, fst, snd } => {
        fst.eta_reduction();
        snd.eta_reduction();
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.eta_reduction();
        for (rule, term) in arms {
          term.eta_reduction();

          if let RulePat::Num(MatchNum::Succ(nam)) = rule {
            let mut lam = Term::Lam { nam: nam.take(), bod: Box::new(std::mem::replace(term, Term::Era)) };
            lam.eta_reduction();
            match lam {
              Term::Lam { nam: nam2, bod } => {
                *nam = nam2;
                *term = *bod;
              }
              body => {
                *rule = RulePat::Num(MatchNum::Zero);
                *term = body;
              }
            }
          }
        }
      }
      Term::Lnk { .. } | Term::Var { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}
    }
  }
}
