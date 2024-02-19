use std::collections::BTreeSet;

use indexmap::IndexSet;
use itertools::Itertools;

use crate::term::{Name, Term};

use super::extract_adt_matches::MatchError;

/// Converts free vars inside the match arms into lambdas with applications to give them the external value.
/// Makes the rules extractable and linear (no need for dups when variable used in both rules)
pub fn linearize_match_free_vars(match_term: &mut Term) -> &mut Term {
  let Term::Mat { matched: _, arms } = match_term else { unreachable!() };
  // Collect the vars.
  // We need consistent iteration order.
  let free_vars: BTreeSet<Name> = arms
    .iter()
    .flat_map(|(pat, term)| term.free_vars().into_keys().filter(|k| !pat.names().contains(k)))
    .collect();

  // Add lambdas to the arms
  for (_, body) in arms {
    let old_body = std::mem::take(body);
    *body = free_vars.iter().rev().fold(old_body, |body, var| Term::named_lam(var.clone(), body));
  }

  // Add apps to the match
  let old_match = std::mem::take(match_term);
  *match_term = free_vars.into_iter().fold(old_match, Term::arg_call);

  // Get a reference to the match again
  // It returns a reference and not an owned value because we want
  //  to keep the new surrounding Apps but still modify the match further.
  let mut match_term = match_term;
  loop {
    match match_term {
      Term::App { tag: _, fun, arg: _ } => match_term = fun.as_mut(),
      Term::Mat { .. } => return match_term,
      _ => unreachable!(),
    }
  }
}

pub fn linearize_match_unscoped_vars(match_term: &mut Term) -> Result<&mut Term, MatchError> {
  let Term::Mat { matched: _, arms } = match_term else { unreachable!() };
  // Collect the vars
  let mut free_vars = IndexSet::new();
  for (_, arm) in arms.iter_mut() {
    let (decls, uses) = arm.unscoped_vars();
    // Not allowed to declare unscoped var and not use it since we need to extract the match arm.
    if let Some(var) = decls.difference(&uses).next() {
      return Err(MatchError::Linearize(format!("λ${var}").into()));
    }
    // Change unscoped var to normal scoped var if it references something outside this match arm.
    let arm_free_vars = uses.difference(&decls);
    for var in arm_free_vars.clone() {
      arm.subst_unscoped(var, &Term::Var { nam: format!("%match%unscoped%{var}").into() });
    }
    free_vars.extend(arm_free_vars.cloned());
  }

  // Add lambdas to the arms
  for (_, body) in arms {
    let old_body = std::mem::take(body);
    *body = free_vars
      .iter()
      .rev()
      .fold(old_body, |body, var| Term::named_lam(format!("%match%unscoped%{var}").into(), body));
  }

  // Add apps to the match
  let old_match = std::mem::take(match_term);
  *match_term = free_vars.into_iter().fold(old_match, |acc, nam| Term::call(acc, [Term::Lnk { nam }]));

  // Get a reference to the match again
  // It returns a reference and not an owned value because we want
  //  to keep the new surrounding Apps but still modify the match further.
  let mut match_term = match_term;
  loop {
    match match_term {
      Term::App { tag: _, fun, arg: _ } => match_term = fun.as_mut(),
      Term::Mat { .. } => return Ok(match_term),
      _ => unreachable!(),
    }
  }
}
