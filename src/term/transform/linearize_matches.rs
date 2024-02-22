use super::extract_adt_matches::{infer_match_type, MatchErr};
use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Pattern, Term, Type},
};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use std::collections::BTreeSet;

impl<'book> Ctx<'book> {
  pub fn linearize_matches(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in &mut self.book.defs {
      for rule in def.rules.iter_mut() {
        let res = rule.body.linearize_matches(&self.book.ctrs);

        self.info.take_err(res, Some(&def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  fn linearize_matches(&mut self, ctrs: &IndexMap<Name, Name>) -> Result<(), MatchErr> {
    match self {
      Term::Mat { matched: box Term::Var { .. }, arms } => {
        for (_, body) in arms.iter_mut() {
          body.linearize_matches(ctrs).unwrap();
        }
        let matched_type = infer_match_type(arms.iter().map(|(x, _)| x), ctrs)?;
        match matched_type {
          // Don't linearize non-adt matches.
          Type::None | Type::Any => (),
          Type::Num => _ = linearize_match_free_vars(self),
          Type::Adt(_) | Type::Tup => {
            let match_term = linearize_match_unscoped_vars(self)?;
            linearize_match_free_vars(match_term);
          }
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.linearize_matches(ctrs)?;
      }

      Term::Let { pat: Pattern::Var(..), val: fst, nxt: snd }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::App { fun: fst, arg: snd, .. } => {
        fst.linearize_matches(ctrs)?;
        snd.linearize_matches(ctrs)?;
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

  get_match_reference(match_term)
}

pub fn linearize_match_unscoped_vars(match_term: &mut Term) -> Result<&mut Term, MatchErr> {
  let Term::Mat { matched: _, arms } = match_term else { unreachable!() };
  // Collect the vars
  let mut free_vars = IndexSet::new();
  for (_, arm) in arms.iter_mut() {
    let (decls, uses) = arm.unscoped_vars();
    // Not allowed to declare unscoped var and not use it since we need to extract the match arm.
    if let Some(var) = decls.difference(&uses).next() {
      return Err(MatchErr::Linearize(format!("λ${var}").into()));
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

  Ok(get_match_reference(match_term))
}

/// Get a reference to the match again
/// It returns a reference and not an owned value because we want
/// to keep the new surrounding Apps but still modify the match further.
fn get_match_reference(mut match_term: &mut Term) -> &mut Term {
  loop {
    match match_term {
      Term::App { tag: _, fun, arg: _ } => match_term = fun.as_mut(),
      Term::Mat { .. } => return match_term,
      _ => unreachable!(),
    }
  }
}
