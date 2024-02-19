use indexmap::IndexMap;

use crate::term::{Book, MatchNum, Name, Op, Pattern, Tag, Term, Type};

use super::{
  extract_adt_matches::{infer_match_type, MatchError},
  linearize_matches,
};

impl Book {
  /// Converts tuple and var matches into let expressions,
  /// makes num matches have exactly one rule for zero and one rule for succ.
  /// Should be run after pattern matching functions are desugared.
  pub fn normalize_native_matches(&mut self) -> Result<(), String> {
    for (def_name, def) in self.defs.iter_mut() {
      def
        .rule_mut()
        .body
        .normalize_native_matches(&self.ctrs)
        .map_err(|e| format!("In definition '{def_name}': {e}"))?;
    }
    Ok(())
  }
}

impl Term {
  fn normalize_native_matches(&mut self, ctrs: &IndexMap<Name, Name>) -> Result<(), MatchError> {
    match self {
      Term::Mat { matched: box Term::Var { nam }, arms } => {
        for (_, body) in arms.iter_mut() {
          body.normalize_native_matches(ctrs)?;
        }
        let matched_type = infer_match_type(arms.iter().map(|(x, _)| x), ctrs)?;
        match matched_type {
          Type::None => {
            return Err(MatchError::Empty);
          }
          // This match is useless, will always go with the first rule.
          // TODO: Throw a warning in this case
          Type::Any => {
            // Inside the match we renamed the variable, so we need to restore the name before the match to remove it.
            let fst_arm = &mut arms[0];
            let Pattern::Var(var) = &fst_arm.0 else { unreachable!() };
            let body = &mut fst_arm.1;
            if let Some(var) = var {
              body.subst(var, &Term::Var { nam: nam.clone() });
            }
            *self = std::mem::take(body);
          }
          // TODO: Don't extract tup matches earlier, only flatten earlier and then deal with them here.
          Type::Tup => unreachable!(),
          // Matching on nums is a primitive operation, we can leave it as is.
          // Not extracting into a separate definition allows us to create very specific inets with the MATCH node.
          Type::Num => {
            // let match_term = linearize_matches::linearize_match_free_vars(self);
            normalize_num_match(self)?;
          }
          Type::Adt(_) => unreachable!("Adt match expressions should have been removed earlier"),
        }
      }
      Term::Mat { .. } => unreachable!("Scrutinee of match expression should have been extracted already"),
      Term::Let { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.normalize_native_matches(ctrs)?;
        snd.normalize_native_matches(ctrs)?;
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.normalize_native_matches(ctrs)?;
      }
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => (),
      Term::Lst { .. } => unreachable!(),
    }
    Ok(())
  }
}

/// Transforms a match on Num with any possible patterns into 'match x {0: ...; +: @x-1 ...}'.
fn normalize_num_match(term: &mut Term) -> Result<(), MatchError> {
  let Term::Mat { matched: _, arms } = term else { unreachable!() };

  let mut zero_arm = None;
  for (pat, body) in arms.iter_mut() {
    match pat {
      Pattern::Num(MatchNum::Zero) => {
        zero_arm = Some((Pattern::Num(MatchNum::Zero), std::mem::take(body)));
        break;
      }
      Pattern::Var(var) => {
        if let Some(var) = var {
          body.subst(var, &Term::Num { val: 0 });
        }
        zero_arm = Some((Pattern::Num(MatchNum::Zero), std::mem::take(body)));
        break;
      }
      Pattern::Num(_) => (),
      _ => unreachable!(),
    }
  }

  let mut succ_arm = None;
  for (pat, body) in arms.iter_mut() {
    match pat {
      // Var already detached.
      // match x {0: ...; +: ...}
      Pattern::Num(MatchNum::Succ(None)) => {
        let body = std::mem::take(body);
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      // Need to detach var.
      // match x {0: ...; +: @x-1 ...}
      Pattern::Num(MatchNum::Succ(Some(var))) => {
        let body = Term::Lam { tag: Tag::Static, nam: var.clone(), bod: Box::new(std::mem::take(body)) };
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      // Need to detach and increment again.
      // match x {0: ...; +: @x-1 let x = (+ x-1 1); ...}
      Pattern::Var(Some(var)) => {
        let body = Term::Let {
          pat: Pattern::Var(Some(var.clone())),
          val: Box::new(Term::Opx {
            op: Op::ADD,
            fst: Box::new(Term::Var { nam: Name::new("%pred") }),
            snd: Box::new(Term::Num { val: 1 }),
          }),
          nxt: Box::new(std::mem::take(body)),
        };

        let body = Term::named_lam(Name::new("%pred"), body);
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      // Var unused, so no need to increment
      // match x {0: ...; +: @* ...}
      Pattern::Var(None) => {
        let body = Term::erased_lam(std::mem::take(body));
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      Pattern::Num(_) => (),
      _ => unreachable!(),
    }
  }

  let Some(zero_arm) = zero_arm else {
    return Err(MatchError::Missing(["0".to_string().into()].into()));
  };
  let Some(succ_arm) = succ_arm else {
    return Err(MatchError::Missing(["+".to_string().into()].into()));
  };
  *arms = vec![zero_arm, succ_arm];
  Ok(())
}
