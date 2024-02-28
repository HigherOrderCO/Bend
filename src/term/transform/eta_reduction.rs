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
      Term::Lam { tag: _, nam: None, bod } => bod.eta_reduction(),

      Term::Mat { args, rules } => {
        for arg in args {
          arg.eta_reduction();
        }
        for rule in rules {
          rule.body.eta_reduction();
        }
      }
      Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
        for el in els {
          el.eta_reduction();
        }
      }
      Term::Let { val: fst, nxt: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.eta_reduction();
        snd.eta_reduction();
      }
      Term::Lnk { .. }
      | Term::Var { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
  }
}
