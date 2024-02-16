use crate::{
  term::{display::DisplayJoin, Book, Definition, Name, Pattern, Term, Type},
  Warning,
};
use indexmap::IndexMap;
use std::collections::HashSet;

impl Book {
  /// Extracts adt match terms into pattern matching functions.
  /// Creates rules with potentially nested patterns, so the flattening pass needs to be called after.
  pub fn extract_adt_matches(&mut self, warnings: &mut Vec<Warning>) -> Result<(), String> {
    let mut new_defs = vec![];
    for (def_name, def) in &mut self.defs {
      for rule in def.rules.iter_mut() {
        rule
          .body
          .extract_adt_matches(def_name, def.builtin, &self.ctrs, &mut new_defs, &mut 0, warnings)
          .map_err(|e| format!("In definition '{def_name}': {e}"))?;
      }
    }
    self.defs.extend(new_defs);
    Ok(())
  }
}

impl Term {
  fn extract_adt_matches(
    &mut self,
    def_name: &Name,
    builtin: bool,
    ctrs: &IndexMap<Name, Name>,
    new_defs: &mut Vec<(Name, Definition)>,
    match_count: &mut usize,
    warnings: &mut Vec<Warning>,
  ) -> Result<(), MatchError> {
    match self {
      Term::Mat { matched: box Term::Var { .. }, arms } => {
        let all_vars = arms.iter().all(|(pat, ..)| matches!(pat, Pattern::Var(..)));
        if all_vars && arms.len() > 1 {
          warnings.push(crate::Warning::MatchOnlyVars { def_name: def_name.clone() });
        }
        for (_, term) in arms.iter_mut() {
          term.extract_adt_matches(def_name, builtin, ctrs, new_defs, match_count, warnings)?;
        }
        Term::linearize_matches(self, def_name, builtin, ctrs, new_defs, match_count)?;
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.extract_adt_matches(def_name, builtin, ctrs, new_defs, match_count, warnings)?;
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.extract_adt_matches(def_name, builtin, ctrs, new_defs, match_count, warnings)?;
        snd.extract_adt_matches(def_name, builtin, ctrs, new_defs, match_count, warnings)?;
      }
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}

      Term::Lst { .. } => unreachable!(),
      Term::Mat { .. } => unreachable!("Scrutinee of match expression should have been extracted already"),
      Term::Let { pat, .. } => {
        unreachable!("Destructor let expression should have been desugared already. {pat}")
      }
    }

    Ok(())
  }
}

//== Common ==//

/// Finds the expected type of the matched argument.
/// Errors on incompatible types.
/// Short-circuits if the first pattern is Type::Any.
pub fn infer_match_type<'a>(
  pats: impl Iterator<Item = &'a Pattern>,
  ctrs: &IndexMap<Name, Name>,
) -> Result<Type, MatchError> {
  let mut match_type = Type::None;
  for pat in pats {
    let new_type = pat.to_type(ctrs);
    match (new_type, &mut match_type) {
      (new, Type::None) => match_type = new,
      // TODO: Should we throw a type inference error in this case?
      // Since anything else will be sort of the wrong type (expected Var).
      (_, Type::Any) => (),
      (Type::Any, _) => (),
      (Type::Tup, Type::Tup) => (),
      (Type::Num, Type::Num) => (),
      (Type::Adt(nam_new), Type::Adt(nam_old)) if &nam_new == nam_old => (),
      (new, old) => {
        return Err(MatchError::Infer(format!("Type mismatch. Found '{}' expected {}.", new, old)));
      }
    };
  }
  Ok(match_type)
}

#[derive(Debug)]
pub enum MatchError {
  Empty,
  Infer(String),
  Repeated(Name),
  Missing(HashSet<Name>),
  LetPat(Box<MatchError>),
  Linearize(Name),
}

impl std::error::Error for MatchError {}

impl std::fmt::Display for MatchError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn ctrs_plural_or_sing(n: usize) -> &'static str {
      if n > 1 { "constructors" } else { "a constructor" }
    }

    match self {
      MatchError::Empty => write!(f, "Empty match block found"),
      MatchError::Infer(err) => write!(f, "{err}"),
      MatchError::Repeated(bind) => write!(f, "Repeated var name in a match block: {}", bind),
      MatchError::Missing(names) => {
        let constructor = ctrs_plural_or_sing(names.len());
        let missing = DisplayJoin(|| names.iter(), ", ");
        write!(f, "Missing {constructor} in a match block: {missing}")
      }
      MatchError::LetPat(err) => {
        let let_err = err.to_string().replace("match block", "let bind");
        write!(f, "{let_err}")?;

        if matches!(err.as_ref(), MatchError::Missing(_)) {
          write!(f, "\nConsider using a match block instead")?;
        }

        Ok(())
      }
      MatchError::Linearize(var) => write!(f, "Unable to linearize variable {var} in a match block."),
    }
  }
}
