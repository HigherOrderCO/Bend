use std::collections::BTreeSet;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;

use crate::term::{Book, Definition, Name, Pattern, Rule, Term, Type};

use super::desugar_match_expressions::{infer_match_type, MatchError};

impl Book {
  pub fn linearize_matches(&mut self) -> Result<(), String> {
    let mut new_defs = vec![];
    for (def_name, def) in &mut self.defs {
      for rule in def.rules.iter_mut() {
        rule
          .body
          .linearize_matches(def_name, def.builtin, &self.ctrs, &mut new_defs, &mut 0)
          .map_err(|e| format!("In definition '{def_name}': {e}"))?;
      }
    }
    self.defs.extend(new_defs);
    Ok(())
  }
}

impl Term {
  pub fn linearize_matches(
    &mut self,
    def_name: &Name,
    builtin: bool,
    ctrs: &IndexMap<Name, Name>,
    new_defs: &mut Vec<(Name, Definition)>,
    match_count: &mut usize,
  ) -> Result<(), MatchError> {
    match self {
      Term::Mat { matched: box Term::Var { .. }, arms } => {
        let matched_type = infer_match_type(arms.iter().map(|(x, _)| x), ctrs)?;
        for (_, term) in arms.iter_mut() {
          term.linearize_matches(def_name, builtin, ctrs, new_defs, match_count)?;
        }
        match matched_type {
          // Don't extract non-adt matches.
          Type::None | Type::Any | Type::Num => (),
          // TODO: Instead of extracting tuple matches, we should flatten one layer and check sub-patterns for something to extract.
          // For now, to prevent extraction we can use `let (a, b) = ...;`
          Type::Adt(_) | Type::Tup => {
            *match_count += 1;
            let match_term = linearize_match_unscoped_vars(self)?;
            let match_term = linearize_match_free_vars(match_term);
            let Term::Mat { matched: box Term::Var { nam }, arms } = match_term else { unreachable!() };
            *match_term = match_to_def(nam, arms, def_name, builtin, new_defs, *match_count);
          }
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.linearize_matches(def_name, builtin, ctrs, new_defs, match_count)?;
      }

      Term::Let { pat: Pattern::Var(..), val: fst, nxt: snd }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::App { fun: fst, arg: snd, .. } => {
        fst.linearize_matches(def_name, builtin, ctrs, new_defs, match_count)?;
        snd.linearize_matches(def_name, builtin, ctrs, new_defs, match_count)?;
      }

      Term::Lst { .. } => unreachable!(),
      Term::Mat { .. } => unreachable!("Scrutinee of match expression should have been extracted already"),
      Term::Let { pat, .. } => {
        unreachable!("Destructor let expression should have been desugared already. {pat}")
      }

      Term::Str { .. }
      | Term::Lnk { .. }
      | Term::Var { .. }
      | Term::Num { .. }
      | Term::Ref { .. }
      | Term::Era => {}

      Term::Err => todo!(),
    };

    Ok(())
  }
}

/// Transforms a match into a new definition with every arm of `arms` as a rule.
/// The result is the new def applied to the scrutinee followed by the free vars of the arms.
fn match_to_def(
  matched_var: &Name,
  arms: &[(Pattern, Term)],
  def_name: &Name,
  builtin: bool,
  new_defs: &mut Vec<(Name, Definition)>,
  match_count: usize,
) -> Term {
  let rules = arms.iter().map(|(pat, term)| Rule { pats: vec![pat.clone()], body: term.clone() }).collect();
  let new_name = Name::from(format!("{def_name}$match${match_count}"));
  let def = Definition { name: new_name.clone(), rules, builtin };
  new_defs.push((new_name.clone(), def));

  Term::arg_call(Term::Ref { nam: new_name }, matched_var.clone())
}

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

fn linearize_match_unscoped_vars(match_term: &mut Term) -> Result<&mut Term, MatchError> {
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
