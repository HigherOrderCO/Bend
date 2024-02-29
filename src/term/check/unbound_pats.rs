use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Pattern, Term},
};
use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone)]
pub struct UnboundCtrErr(Name);

impl Display for UnboundCtrErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Unbound constructor '{}'.", self.0)
  }
}

impl Ctx<'_> {
  /// Check if the constructors in rule patterns or match patterns are defined.
  pub fn check_unbound_pats(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    let is_ctr = |nam: &Name| self.book.ctrs.contains_key(nam);
    for (def_name, def) in self.book.defs.iter() {
      for rule in &def.rules {
        for pat in &rule.pats {
          let res = pat.check_unbounds(&is_ctr);
          self.info.take_err(res, Some(def_name));
        }

        let res = rule.body.check_unbound_pats(&is_ctr);
        self.info.take_err(res, Some(def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Pattern {
  pub fn check_unbounds(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtrErr> {
    let unbounds = self.unbound_pats(is_ctr);
    if let Some(unbound) = unbounds.iter().next() { Err(UnboundCtrErr(unbound.clone())) } else { Ok(()) }
  }

  /// Given a possibly nested rule pattern, return a set of all used but not declared constructors.
  pub fn unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> HashSet<Name> {
    let mut unbounds = HashSet::new();
    let mut check = vec![self];
    while let Some(pat) = check.pop() {
      match pat {
        Pattern::Ctr(nam, args) => {
          if !is_ctr(nam) {
            unbounds.insert(nam.clone());
          }
          check.extend(args.iter());
        }
        Pattern::Tup(fst, snd) => {
          check.push(fst);
          check.push(snd);
        }
        Pattern::Lst(args) => args.iter().for_each(|arg| check.push(arg)),
        Pattern::Var(_) | Pattern::Num(_) | Pattern::Str(_) => {}
      }
    }
    unbounds
  }
}

impl Term {
  pub fn check_unbound_pats(&self, is_ctr: &impl Fn(&Name) -> bool) -> Result<(), UnboundCtrErr> {
    stacker::maybe_grow(1024 * 32, 1024 * 1024, move || {
      match self {
        Term::Let { pat, val, nxt } => {
          pat.check_unbounds(is_ctr)?;
          val.check_unbound_pats(is_ctr)?;
          nxt.check_unbound_pats(is_ctr)?;
        }
        Term::Mat { args, rules } => {
          for arg in args {
            arg.check_unbound_pats(is_ctr)?;
          }
          for rule in rules {
            for pat in &rule.pats {
              pat.check_unbounds(is_ctr)?;
            }
            rule.body.check_unbound_pats(is_ctr)?;
          }
        }
        Term::App { fun: fst, arg: snd, .. }
        | Term::Tup { fst, snd }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Sup { fst, snd, .. }
        | Term::Opx { fst, snd, .. } => {
          fst.check_unbound_pats(is_ctr)?;
          snd.check_unbound_pats(is_ctr)?;
        }
        Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.check_unbound_pats(is_ctr)?,
        Term::Lst { .. } => unreachable!(),
        Term::Var { .. }
        | Term::Lnk { .. }
        | Term::Ref { .. }
        | Term::Num { .. }
        | Term::Str { .. }
        | Term::Era
        | Term::Err => (),
      }
      Ok(())
    })
  }
}
