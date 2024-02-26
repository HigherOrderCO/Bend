use super::encode_pattern_matching::MatchErr;
use crate::{
  diagnostics::Info,
  term::{check::type_check::infer_type, Constructors, Ctx, Name, Pattern, Term, Type},
};
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::{BTreeMap, BTreeSet};

impl Ctx<'_> {
  /// Linearizes the variables between match cases, transforming them into combinators when possible.
  pub fn linearize_matches(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        let res = rule.body.linearize_matches(&self.book.ctrs);
        self.info.take_err(res, Some(def_name));
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  fn linearize_matches(&mut self, ctrs: &Constructors) -> Result<(), MatchErr> {
    match self {
      Term::Mat { args: _, rules } => {
        for rule in rules.iter_mut() {
          rule.body.linearize_matches(ctrs).unwrap();
        }
        let matched_type = infer_type(rules.iter().map(|r| &r.pats[0]), ctrs)?;
        match matched_type {
          Type::Num | Type::Tup | Type::Any => _ = linearize_match_free_vars(self),
          Type::Adt(_) => {
            linearize_match_free_vars(self);
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
  let Term::Mat { args: _, rules } = match_term else { unreachable!() };
  // Collect the vars.
  // We need consistent iteration order.

  let mut acc: BTreeMap<Name, u64> = BTreeMap::new();
  rules.iter().for_each(|r| {
    let fvs = r.body.free_vars().into_iter();
    for (k, v) in fvs {
      if !r.pats.iter().flat_map(|p| p.binds()).contains(&k) {
        // Counts the number of arms that the var is used
        *acc.entry(k).or_insert(0) += u64::min(v, 1);
      }
    }
  });
  let free_vars: BTreeSet<Name> = acc.into_iter().filter(|(_, v)| *v >= 2).map(|(k, _)| k).collect();

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

pub fn linearize_match_unscoped_vars(match_term: &mut Term) -> Result<&mut Term, MatchErr> {
  let Term::Mat { args: _, rules } = match_term else { unreachable!() };
  // Collect the vars
  let mut free_vars = IndexSet::new();
  for rule in rules.iter_mut() {
    let (decls, uses) = rule.body.unscoped_vars();
    // Not allowed to declare unscoped var and not use it since we need to extract the match arm.
    if let Some(var) = decls.difference(&uses).next() {
      return Err(MatchErr::Linearize(Name::new(format!("λ${var}"))));
    }
    // Change unscoped var to normal scoped var if it references something outside this match arm.
    let arm_free_vars = uses.difference(&decls);
    for var in arm_free_vars.clone() {
      rule.body.subst_unscoped(var, &Term::Var { nam: Name::new(format!("%match%unscoped%{var}")) });
    }
    free_vars.extend(arm_free_vars.cloned());
  }

  // Add lambdas to the arms
  for rule in rules {
    let old_body = std::mem::take(&mut rule.body);
    rule.body = free_vars
      .iter()
      .rev()
      .fold(old_body, |body, var| Term::named_lam(Name::new(format!("%match%unscoped%{var}")), body));
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
