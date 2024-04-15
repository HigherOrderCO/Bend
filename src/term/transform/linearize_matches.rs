use crate::{
  maybe_grow,
  term::{Book, Name, Term},
};
use std::collections::{BTreeSet, HashSet};

/* Linearize preceding binds */

impl Book {
  /// Linearization of binds preceding match/switch terms, up to the
  /// first bind used in either the scrutinee or the bind.
  ///
  /// Example:
  /// ```hvm
  /// @a @b @c let d = (b c); switch a {
  ///   0: (A b c d)
  ///   _: (B a-1 b c d)
  /// }
  /// // Since `b`, `c` and `d` would be eta-reducible if linearized,
  /// // they get pushed inside the match.
  /// @a switch a {
  ///   0: @b @c let d = (b c); (A b c d)
  ///   _: @b @c let d = (b c); (B a-1 b c d)
  /// }
  /// ```
  pub fn linearize_match_binds(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_match_binds();
      }
    }
  }
}

impl Term {
  /// Linearize any binds preceding a match/switch term, up to the
  /// first bind used in either the scrutinee or the bind.
  pub fn linearize_match_binds(&mut self) {
    self.linearize_match_binds_go(vec![]);
  }

  fn linearize_match_binds_go(&mut self, mut bind_terms: Vec<Term>) {
    match self {
      // Binding terms
      // Extract them in case they are preceding a match.
      Term::Lam { bod, .. } => {
        let bod = std::mem::take(bod.as_mut());
        let term = std::mem::replace(self, bod);
        bind_terms.push(term);
        self.linearize_match_binds_go(bind_terms);
      }
      Term::Let { val, nxt, .. }
      | Term::Ltp { val, nxt, .. }
      | Term::Dup { val, nxt, .. }
      | Term::Use { val, nxt, .. } => {
        val.linearize_match_binds_go(vec![]);
        let nxt = std::mem::take(nxt.as_mut());
        let term = std::mem::replace(self, nxt);
        bind_terms.push(term);
        self.linearize_match_binds_go(bind_terms);
      }

      // Matching terms
      Term::Mat { .. } | Term::Swt { .. } => {
        self.linearize_match(bind_terms);
      }

      // Others
      // Not a match preceded by binds, so put the extracted terms back.
      term => {
        for child in term.children_mut() {
          child.linearize_match_binds_go(vec![]);
        }
        // Recover the extracted terms
        term.wrap_with_bind_terms(bind_terms);
      }
    }
  }

  fn linearize_match(&mut self, mut bind_terms: Vec<Term>) {
    // The used vars are any free vars in the argument,
    // the match bind and the ctr field binds.
    let (vars, with, mut arms) = match self {
      Term::Mat { arg, bnd, with, arms } => {
        let mut vars = arg.free_vars().into_keys().collect::<HashSet<_>>();
        if let Some(bnd) = bnd {
          vars.insert(bnd.clone());
        }
        for arm in arms.iter() {
          vars.extend(arm.1.iter().flatten().cloned());
        }

        let arms = arms.iter_mut().map(|arm| &mut arm.2).collect::<Vec<_>>();

        (vars, with, arms)
      }
      Term::Swt { arg, bnd, with, pred, arms } => {
        let mut vars = arg.free_vars().into_keys().collect::<HashSet<_>>();
        if let Some(bnd) = bnd {
          vars.insert(bnd.clone());
        }
        if let Some(pred) = pred {
          vars.insert(pred.clone());
        }

        let arms = arms.iter_mut().collect();

        (vars, with, arms)
      }
      _ => unreachable!(),
    };

    // Move binding terms inwards, up to the first bind used in the match.
    while let Some(term) = bind_terms.pop() {
      // Get the binds in the term we want to push down.
      let binds: Vec<Name> = match &term {
        Term::Dup { bnd, .. } | Term::Ltp { bnd, .. } => bnd.iter().flatten().cloned().collect(),
        Term::Lam { nam, .. } | Term::Let { nam, .. } | Term::Use { nam, .. } => {
          if let Some(nam) = nam {
            vec![nam.clone()]
          } else {
            vec![]
          }
        }
        _ => unreachable!(),
      };

      if binds.iter().all(|bnd| !vars.contains(bnd)) {
        // If possible, move term inside, wrapping around each arm.
        for arm in arms.iter_mut() {
          arm.wrap_with_bind_terms([term.clone()]);
        }
        // Since this bind doesn't exist anymore,
        // we have to remove it from the `with` clause.
        with.retain(|var| !binds.contains(var));
      } else {
        // Otherwise we stop and put this term back to be put in it's original position.
        bind_terms.push(term);
        break;
      }
    }

    // Recurse
    for arm in arms {
      arm.linearize_match_binds_go(vec![]);
    }

    // Recover any leftover bind terms that were not moved
    self.wrap_with_bind_terms(bind_terms);
  }

  /// Given a term `self` and a sequence of `bind_terms`, wrap `self` with those binds.
  ///
  /// Example:
  /// ```hvm
  /// self = X
  /// match_terms = [λb *, let c = (a b); *, λd *]
  /// ```
  ///
  /// becomes
  ///
  /// ```hvm
  /// self = λb let c = (a b); λd X
  /// ```
  fn wrap_with_bind_terms(
    &mut self,
    bind_terms: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = Term>>,
  ) {
    *self = bind_terms.into_iter().rfold(std::mem::take(self), |acc, mut term| {
      match &mut term {
        Term::Lam { bod: nxt, .. }
        | Term::Let { nxt, .. }
        | Term::Ltp { nxt, .. }
        | Term::Dup { nxt, .. }
        | Term::Use { nxt, .. } => {
          *nxt.as_mut() = acc;
        }
        _ => unreachable!(),
      }
      term
    });
  }
}

/* Linearize all used vars */

impl Book {
  /// Linearizes all variables used in a matches' arms.
  pub fn linearize_matches(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_matches();
      }
    }
  }
}

impl Term {
  fn linearize_matches(&mut self) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.linearize_matches();
      }

      if matches!(self, Term::Mat { .. } | Term::Swt { .. }) {
        lift_match_vars(self);
      }
    })
  }
}

/// Converts free vars inside the match arms into lambdas with
/// applications around the match to pass them the external value.
///
/// Makes the rules extractable and linear (no need for dups even
/// when a variable is used in multiple rules).
///
/// Obs: This does not modify unscoped variables.
pub fn lift_match_vars(match_term: &mut Term) -> &mut Term {
  // Collect match arms with binds
  let arms: Vec<_> = match match_term {
    Term::Mat { arg: _, bnd: _, with: _, arms: rules } => {
      rules.iter().map(|(_, binds, body)| (binds.iter().flatten().cloned().collect(), body)).collect()
    }
    Term::Swt { arg: _, bnd: _, with: _, pred, arms } => {
      let (succ, nums) = arms.split_last_mut().unwrap();
      let mut arms = nums.iter().map(|body| (vec![], body)).collect::<Vec<_>>();
      arms.push((vec![pred.clone().unwrap()], succ));
      arms
    }
    _ => unreachable!(),
  };

  // Collect all free vars in the match arms
  let mut free_vars = Vec::<Vec<_>>::new();
  for (binds, body) in arms {
    let mut arm_free_vars = body.free_vars();
    for bind in binds {
      arm_free_vars.remove(&bind);
    }
    free_vars.push(arm_free_vars.into_keys().collect());
  }

  // Collect the vars to lift
  // We need consistent iteration order.
  let vars_to_lift: BTreeSet<Name> = free_vars.into_iter().flatten().collect();

  // Add lambdas to the arms
  match match_term {
    Term::Mat { arg: _, bnd: _, with, arms } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for arm in arms {
        let old_body = std::mem::take(&mut arm.2);
        arm.2 = vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::named_lam(nam, body));
      }
    }
    Term::Swt { arg: _, bnd: _, with, pred: _, arms } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for arm in arms {
        let old_body = std::mem::take(arm);
        *arm = vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::named_lam(nam, body));
      }
    }
    _ => unreachable!(),
  }

  // Add apps to the match
  *match_term = vars_to_lift.into_iter().fold(std::mem::take(match_term), Term::arg_call);

  get_match_reference(match_term)
}

/// Get a reference to the match again
/// It returns a reference and not an owned value because we want
/// to keep the new surrounding Apps but still modify the match further.
fn get_match_reference(mut match_term: &mut Term) -> &mut Term {
  loop {
    match match_term {
      Term::App { tag: _, fun, arg: _ } => match_term = fun.as_mut(),
      Term::Swt { .. } | Term::Mat { .. } => return match_term,
      _ => unreachable!(),
    }
  }
}

/* Linearize `with` vars  */

impl Book {
  /// Linearizes all variables specified in the `with` clauses of match terms.
  pub fn linearize_match_with(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_match_with();
      }
    }
  }
}

impl Term {
  fn linearize_match_with(&mut self) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.linearize_match_with();
      }
    });
    match self {
      Term::Mat { arg: _, bnd: _, with, arms: rules } => {
        // Linearize the vars in the `with` clause, but only the used ones.
        let with = retain_used_names(std::mem::take(with), rules.iter().map(|r| &r.2));
        for rule in rules {
          rule.2 =
            with.iter().rfold(std::mem::take(&mut rule.2), |bod, nam| Term::lam(Some(nam.clone()), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      Term::Swt { arg: _, bnd: _, with, pred: _, arms } => {
        let with = retain_used_names(std::mem::take(with), arms.iter());
        for arm in arms {
          *arm = with.iter().rfold(std::mem::take(arm), |bod, nam| Term::lam(Some(nam.clone()), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      _ => {}
    }
  }
}

/// From a Vec of variable names, return the ones that are used inside `terms`.
fn retain_used_names<'a>(mut names: Vec<Name>, terms: impl IntoIterator<Item = &'a Term>) -> Vec<Name> {
  let mut used_names = HashSet::new();
  for term in terms.into_iter() {
    let mut free_vars = term.free_vars();
    free_vars.retain(|_, uses| *uses > 0);
    used_names.extend(free_vars.into_keys());
  }
  names.retain(|nam| used_names.contains(nam));
  names
}
