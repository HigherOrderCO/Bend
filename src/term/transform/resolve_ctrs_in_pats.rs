use crate::term::{Book, Name, Pattern, Term};

impl Book {
  /// Resolve Constructor names inside rule patterns and match patterns.
  /// When parsing a rule we don't have all the constructors yet,
  /// so no way to know if a particular name belongs to a constructor or is a matched variable.
  /// Therefore we must do it later, here.
  pub fn resolve_ctrs_in_pats(&mut self) {
    let is_ctr = |nam: &Name| self.ctrs.contains_key(nam);
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        for pat in &mut rule.pats {
          pat.resolve_ctrs(&is_ctr);
        }
        rule.body.resolve_ctrs_in_pats(&is_ctr);
      }
    }
  }
}

impl Pattern {
  pub fn resolve_ctrs(&mut self, is_ctr: &impl Fn(&Name) -> bool) {
    let mut to_resolve = vec![self];

    while let Some(pat) = to_resolve.pop() {
      match pat {
        Pattern::Var(Some(nam)) => {
          if is_ctr(nam) {
            *pat = Pattern::Ctr(nam.clone(), vec![]);
          }
        }
        Pattern::Ctr(_, args) | Pattern::Lst(args) => {
          for arg in args {
            to_resolve.push(arg);
          }
        }
        Pattern::Var(None) => (),
        Pattern::Num(_) => (),
        Pattern::Str(_) => (),
        Pattern::Tup(fst, snd) => {
          to_resolve.push(fst);
          to_resolve.push(snd);
        }
      }
    }
  }
}

impl Term {
  pub fn resolve_ctrs_in_pats(&mut self, is_ctr: &impl Fn(&Name) -> bool) {
    let mut to_resolve = vec![self];

    while let Some(pat) = to_resolve.pop() {
      match pat {
        Term::Let { pat, val, nxt } => {
          pat.resolve_ctrs(is_ctr);
          to_resolve.push(val);
          to_resolve.push(nxt);
        }
        Term::Mat { args, rules } => {
          for arg in args {
            to_resolve.push(arg);
          }
          for rule in rules {
            for pat in &mut rule.pats {
              pat.resolve_ctrs(is_ctr);
            }
            to_resolve.push(&mut rule.body);
          }
        }
        Term::App { fun: fst, arg: snd, .. }
        | Term::Tup { fst, snd }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Sup { fst, snd, .. }
        | Term::Opx { fst, snd, .. } => {
          to_resolve.push(fst);
          to_resolve.push(snd);
        }
        Term::Lam { bod, .. } | Term::Chn { bod, .. } => to_resolve.push(bod),
        Term::Var { .. }
        | Term::Lnk { .. }
        | Term::Ref { .. }
        | Term::Num { .. }
        | Term::Str { .. }
        | Term::Lst { .. }
        | Term::Era
        | Term::Err => (),
      }
    }
  }
}
