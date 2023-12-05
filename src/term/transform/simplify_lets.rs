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
    if let Term::Let { mut pat, mut val, mut nxt } = self.clone() {
      let mut extracted = extract(&mut pat);
      extracted.reverse();
      while let Some((nam, extracted)) = extracted.pop() {
        let val = Box::new(Term::Var { nam });
        nxt = Box::new(Term::Let { pat: extracted, val, nxt })
      }
      val.simplify_let();
      nxt.simplify_let();
      *self = Term::Let { pat, val, nxt }
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
