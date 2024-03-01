use super::encode_pattern_matching::MatchErr;
use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Pattern, Term},
};
use itertools::Itertools;
use std::collections::{BTreeMap, BTreeSet};

impl Ctx<'_> {
  /// Linearizes the variables between match cases, transforming them into combinators when possible.
  pub fn linearize_simple_matches(&mut self, lift_all_vars: bool) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        let res = rule.body.linearize_simple_matches(lift_all_vars);
        self.info.take_err(res, Some(def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  fn linearize_simple_matches(&mut self, lift_all_vars: bool) -> Result<(), MatchErr> {
    stacker::maybe_grow(1024 * 32, 1024 * 1024, move || {
      match self {
        Term::Mat { args: _, rules } => {
          for rule in rules.iter_mut() {
            rule.body.linearize_simple_matches(lift_all_vars).unwrap();
          }
          lift_match_vars(self, lift_all_vars);
        }

        Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
          bod.linearize_simple_matches(lift_all_vars)?;
        }
        Term::Let { pat: Pattern::Var(..), val: fst, nxt: snd }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Opx { fst, snd, .. }
        | Term::App { fun: fst, arg: snd, .. } => {
          fst.linearize_simple_matches(lift_all_vars)?;
          snd.linearize_simple_matches(lift_all_vars)?;
        }
        Term::Let { pat, .. } => {
          unreachable!("Destructor let expression should have been desugared already. {pat}")
        }
        Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
          for el in els {
            el.linearize_simple_matches(lift_all_vars)?;
          }
        }
        Term::Str { .. }
        | Term::Lnk { .. }
        | Term::Var { .. }
        | Term::Num { .. }
        | Term::Ref { .. }
        | Term::Era => {}

        Term::Err => unreachable!(),
      }

      Ok(())
    })
  }
}

/// Converts free vars inside the match arms into lambdas with applications to give them the external value.
/// Makes the rules extractable and linear (no need for dups when variable used in both rules)
///
/// If `lift_all_vars`, acts on all variables found in the arms,
/// Otherwise, only lift vars that are used on more than one arm.
///
/// Obs: This does not interact unscoped variables
pub fn lift_match_vars(match_term: &mut Term, lift_all_vars: bool) -> &mut Term {
  let Term::Mat { args: _, rules } = match_term else { unreachable!() };

  let free = rules.iter().flat_map(|rule| {
    rule.body.free_vars().into_iter().filter(|(name, _)| !rule.pats.iter().any(|p| p.binds().contains(name)))
  });

  // Collect the vars.
  // We need consistent iteration order.
  let free_vars: BTreeSet<Name> = if lift_all_vars {
    free.map(|(name, _)| name).collect()
  } else {
    free
      .fold(BTreeMap::new(), |mut acc, (name, count)| {
        *acc.entry(name).or_insert(0) += count.min(1);
        acc
      })
      .into_iter()
      .filter(|(_, count)| *count >= 2)
      .map(|(name, _)| name)
      .collect()
  };

  // Add lambdas to the arms
  for rule in rules {
    let old_body = std::mem::take(&mut rule.body);
    rule.body = free_vars.iter().rev().fold(old_body, |body, var| Term::named_lam(var.clone(), body));
  }

  // Add apps to the match
  let old_match = std::mem::take(match_term);
  *match_term = free_vars.into_iter().fold(old_match, Term::arg_call);

  get_match_reference(match_term)
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
