use crate::term::{Book, Name, NumCtr, Tag, Term};
use std::collections::{BTreeSet, HashSet};

impl Book {
  /// Linearization of only eta-reducible match variables.
  ///
  /// Example:
  /// ```hvm
  /// @a @b @c switch a {
  ///   0: (A b c)
  ///   _: (B a-1 b c)
  /// }
  /// // Since `b` and `c` would be eta-reducible if linearized,
  /// // they get pushed inside the match.
  /// @a switch a {
  ///   0: @b @c (A b c)
  ///   _: @b @c (B a-1 b c)
  /// }
  /// ```
  pub fn linearize_match_lambdas(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_match_lambdas();
      }
    }
  }

  /// Linearizes all variables used in a matches' arms.
  pub fn linearize_matches(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_matches();
      }
    }
  }

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
  /// Linearize any lambdas preceding a match/switch expression.
  ///
  /// We push down the lambdas between the one being matched and the match expression.
  ///
  /// ### Example:
  /// ```hvm
  /// @a @b @c @d switch b {
  ///   0: (Foo a c d)
  ///   _: (Bar a b-1 c d)
  /// }
  ///
  /// // Becomes
  /// @a @b switch b {
  ///   0: @c @d (Foo a c d)
  ///   _: @c @d (Bar a b-1 c d)
  /// }
  /// ```
  pub fn linearize_match_lambdas(&mut self) {
    Term::recursive_call(move || match self {
      Term::Lam { .. } => {
        let mut term_owned = std::mem::take(self);
        let mut term = &mut term_owned;
        let mut lams = vec![];
        while let Term::Lam { tag, nam, bod } = term {
          lams.push((tag, nam));
          term = bod.as_mut();
        }

        let to_linearize = match term {
          Term::Mat { arg, with, rules } if matches!(arg.as_ref(), Term::Var { .. } | Term::Era) => {
            let arg = if let Term::Var { nam } = arg.as_ref() { Some(nam.clone()) } else { None };
            let bodies = rules.iter_mut().map(|(_, _, body)| body).collect::<Vec<_>>();
            Some((arg, with, bodies))
          }
          Term::Swt { arg, with, rules } if matches!(arg.as_ref(), Term::Var { .. } | Term::Era) => {
            let arg = if let Term::Var { nam } = arg.as_ref() { Some(nam.clone()) } else { None };
            let bodies = rules.iter_mut().map(|(_, body)| body).collect();
            Some((arg, with, bodies))
          }
          _ => None,
        };

        if let Some((arg, with, mut bodies)) = to_linearize {
          // Lambdas followed by match, push the lambdas inside the match arms.
          // Find which lambdas will be pushed down and which have to remain.
          let mut last_lam = 0;
          for i in (0 .. lams.len()).rev() {
            if let Some(lam_var) = &lams[i].1
              && *lam_var == arg
            {
              last_lam = i + 1;
              break;
            }
          }

          for (tag, nam) in lams[last_lam ..].iter_mut().rev() {
            // Move the lambdas down
            for body in bodies.iter_mut() {
              **body = Term::Lam { tag: tag.clone(), nam: nam.clone(), bod: Box::new(std::mem::take(body)) };
            }
            // Remove the variable from the with clause
            if let Some(nam) = nam {
              with.retain(|with| with != nam);
            }
          }

          // Recursive call on the moved lambdas
          for body in bodies.iter_mut() {
            body.linearize_match_lambdas();
          }

          // Reconstruct the lambdas that were not moved down
          term_owned =
            lams[0 .. last_lam].iter_mut().rfold(std::mem::take(term), |term, (tag, nam)| Term::Lam {
              tag: std::mem::replace(tag, Tag::Static),
              nam: std::mem::take(nam),
              bod: Box::new(term),
            });
        } else {
          // Not an interesting match, go back to the normal flow
          term.linearize_match_lambdas()
        }

        *self = term_owned;
      }
      _ => {
        for child in self.children_mut() {
          child.linearize_match_lambdas();
        }
      }
    })
  }

  fn linearize_matches(&mut self) {
    Term::recursive_call(move || {
      for child in self.children_mut() {
        child.linearize_matches();
      }

      if matches!(self, Term::Mat { .. } | Term::Swt { .. }) {
        lift_match_vars(self);
      }
    })
  }

  fn linearize_match_with(&mut self) {
    Term::recursive_call(|| {
      for child in self.children_mut() {
        child.linearize_match_with();
      }
    });
    match self {
      Term::Mat { arg: _, with, rules } => {
        // Linearize the vars in the `with` clause, but only the used ones.
        let with = retain_used_names(std::mem::take(with), rules.iter().map(|r| &r.2));
        for rule in rules {
          rule.2 =
            with.iter().rfold(std::mem::take(&mut rule.2), |bod, nam| Term::lam(Some(nam.clone()), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      Term::Swt { arg: _, with, rules } => {
        let with = retain_used_names(std::mem::take(with), rules.iter().map(|r| &r.1));
        for rule in rules {
          rule.1 =
            with.iter().rfold(std::mem::take(&mut rule.1), |bod, nam| Term::lam(Some(nam.clone()), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      _ => {}
    }
  }
}

/// Converts free vars inside the match arms into lambdas with applications to give them the external value.
/// Makes the rules extractable and linear (no need for dups when variable used in both rules)
///
/// If `lift_all_vars`, acts on all variables found in the arms,
/// Otherwise, only lift vars that are used on more than one arm.
///
/// Obs: This does not modify unscoped variables
pub fn lift_match_vars(match_term: &mut Term) -> &mut Term {
  // Collect match arms with binds
  let arms: Vec<_> = match match_term {
    Term::Mat { arg: _, with: _, rules } => {
      rules.iter().map(|(_, binds, body)| (binds.iter().flatten().cloned().collect(), body)).collect()
    }
    Term::Swt { arg: _, with: _, rules } => rules
      .iter()
      .map(|(ctr, body)| match ctr {
        NumCtr::Num(_) => (vec![], body),
        NumCtr::Succ(None) => (vec![], body),
        NumCtr::Succ(Some(var)) => (vec![var.clone()], body),
      })
      .collect(),
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
    Term::Mat { arg: _, with, rules } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for rule in rules {
        let old_body = std::mem::take(&mut rule.2);
        rule.2 = vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::named_lam(nam, body));
      }
    }
    Term::Swt { arg: _, with, rules } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for rule in rules {
        let old_body = std::mem::take(&mut rule.1);
        rule.1 = vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::named_lam(nam, body));
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
