use crate::term::*;

impl Book {
  /// Applies eta-reduction to all generated definitions, converting occurrences of `@x (f x)` into just `f`.
  /// Assumes that variables are linear (used exactly once).
  pub fn eta_reduction(&mut self) {
    for def in self.defs.values_mut() {
      def.rule_mut().body.eta_reduction();
    }
  }
}

impl Term {
  /// Eta-reduces a term and any subterms.
  /// Expects variables to be linear.
  pub fn eta_reduction(&mut self) {
    match self {
      Term::Lam { tag: lam_tag, nam: Some(lam_var), bod } => {
        bod.eta_reduction();
        match bod.as_mut() {
          Term::App { tag: arg_tag, fun, arg: box Term::Var { nam: var_nam } }
            if lam_var == var_nam && lam_tag == arg_tag =>
          {
            *self = std::mem::take(fun.as_mut());
          }
          _ => {}
        }
      }
      Term::Chn { tag: chn_tag, nam: chn_var, bod } => {
        bod.eta_reduction();
        match bod.as_mut() {
          Term::App { tag: arg_tag, fun, arg: box Term::Lnk { nam: var_nam } }
            if chn_var == var_nam && chn_tag == arg_tag =>
          {
            *self = std::mem::take(fun.as_mut());
          }
          _ => {}
        }
      }
      Term::Lam { bod, .. } => bod.eta_reduction(),
      Term::Let { pat: _, val, nxt } | Term::Dup { tag: _, fst: _, snd: _, val, nxt } => {
        val.eta_reduction();
        nxt.eta_reduction();
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { op: _, fst, snd } => {
        fst.eta_reduction();
        snd.eta_reduction();
      }
      Term::Mat { args, rules } => {
        for arg in args {
          arg.eta_reduction();
        }
        for rule in rules {
          for pat in &rule.pats {
            debug_assert!(pat.is_native_num_match());
          }
          rule.body.eta_reduction();
        }
      }
      Term::Lnk { .. }
      | Term::Var { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Lst { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
  }
}
