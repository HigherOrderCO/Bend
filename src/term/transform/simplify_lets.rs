use crate::term::{Book, Name, Pattern, Term};

fn extract_tup(is_fst: bool, count: &mut usize, pat: &mut Pattern, paths: &mut Vec<(Name, Pattern)>) {
  if let Pattern::Tup(fst, snd) = pat {
    extract_tup(false, count, fst, paths);
    extract_tup(false, count, snd, paths);
    if !is_fst {
      let old_pat = pat.clone();
      let name = Name(format!("%x{count}"));
      *count += 1;
      *pat = Pattern::Var(Some(name.clone()));
      paths.push((name, old_pat));
    }
  }
}

fn extract(pat: &mut Pattern) -> Vec<(Name, Pattern)> {
  let mut ret = Vec::new();
  extract_tup(true, &mut 0, pat, &mut ret);
  ret
}

impl Term {
  pub fn simplify_let(&mut self) {
    match self {
      Term::Let { .. } => {
        let Term::Let { mut pat, mut val, mut nxt } = std::mem::take(self) else { unreachable!() };
        let mut extracted = extract(&mut pat);
        extracted.reverse();
        while let Some((nam, extracted)) = extracted.pop() {
          let val = Box::new(Term::Var { nam });
          nxt = Box::new(Term::Let { pat: extracted, val, nxt })
        }
        val.simplify_let();
        nxt.simplify_let();
        *self = Term::Let { pat, val, nxt }
      },
      Term::Dup { val, nxt, .. } => {
        val.simplify_let();
        nxt.simplify_let();
      },
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.simplify_let(),
      Term::App { fun, arg } => {
        fun.simplify_let();
        arg.simplify_let();
      },
      Term::Tup { fst, snd } => {
        fst.simplify_let();
        snd.simplify_let();
      },
      Term::Sup { fst, snd, .. } | Term::Opx { fst, snd, .. } => {
        fst.simplify_let();
        snd.simplify_let()
      },
      Term::Match { arms, .. } => {
        for arm in arms {
          arm.1.simplify_let();
        }
      },
      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => (),
    }
  }
}

impl Book {
  pub fn simplify_lets(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.simplify_let();
      }
    }
  }
}
