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
    Term::recursive_call(move || match self {
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

      _ => {
        for child in self.children_mut() {
          child.eta_reduction()
        }
      }
    })
  }
}
