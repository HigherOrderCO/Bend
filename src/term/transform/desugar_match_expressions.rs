use crate::term::{
  display::DisplayJoin, Book, DefId, DefNames, Definition, MatchNum, Name, Op, Pattern, Rule, Tag, Term, Type,
};
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  /// Extracts any match terms into pattern matching functions.
  /// Creates rules with potentially nested patterns, so the flattening pass needs to be called after.
  pub fn extract_matches(&mut self) -> Result<(), String> {
    let book = &mut MatchesBook::new(&mut self.def_names);

    for (def_id, def) in &mut self.defs {
      let def_name = book.def_names.name(def_id).cloned().unwrap();
      for rule in def.rules.iter_mut() {
        rule.body.extract_matches(&def_name, &self.ctrs, book, &mut 0).map_err(|err| err.to_string())?;
      }
    }

    self.defs.append(&mut book.new_defs);
    Ok(())
  }
}

struct MatchesBook<'book> {
  def_names: &'book mut DefNames,
  new_defs: BTreeMap<DefId, Definition>,
}

impl<'book> MatchesBook<'book> {
  fn new(def_names: &'book mut DefNames) -> Self {
    Self { def_names, new_defs: BTreeMap::<DefId, Definition>::new() }
  }
}

#[derive(Debug)]
pub enum MatchError {
  Empty,
  Infer(String),
  Repeated(Name),
  Missing(HashSet<Name>),
  LetPat(Box<MatchError>),
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
    }
  }
}

impl Term {
  fn extract_matches(
    &mut self,
    def_name: &Name,
    ctrs: &HashMap<Name, Name>,
    book: &mut MatchesBook,
    match_count: &mut usize,
  ) -> Result<(), MatchError> {
    match self {
      Term::Match { scrutinee: box Term::Var { nam }, arms } => {
        if arms.is_empty() {
          return Err(MatchError::Empty);
        }

        for (_, term) in arms.iter_mut() {
          term.extract_matches(def_name, ctrs, book, match_count)?;
        }

        let matched_type = infer_match_type(arms.iter().map(|(x, _)| x), ctrs)?;
        match matched_type {
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
          // Matching on nums is a primitive operation, we can leave it as is.
          // Not extracting into a separate definition allows us to create very specific inets with the MATCH node.
          Type::Num => normalize_num_match(self)?,
          // TODO: It would be nice to also not extract tuple matches,
          // but if they have nested patterns it gets more complicated.
          // For now, we use `let (a, b) = x` to get the specific desired inet.
          _ => {
            *match_count += 1;
            let nam = std::mem::take(nam);
            let arms = std::mem::take(arms);
            *self = match_to_def(nam, &arms, def_name, book, *match_count);
          }
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.extract_matches(def_name, ctrs, book, match_count)?;
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.extract_matches(def_name, ctrs, book, match_count)?;
        snd.extract_matches(def_name, ctrs, book, match_count)?;
      }
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era => {}

      Term::List { .. } => unreachable!(),
      Term::Match { .. } => unreachable!("Scrutinee of match expression should have been extracted already"),
      Term::Let { pat, .. } => {
        unreachable!("Destructor let expression should have been desugared already. {pat}")
      }
    }

    Ok(())
  }
}

/// Transforms a match into a new definition with every arm of `arms` as a rule.
/// The result is the new def applied to the scrutinee followed by the free vars of the arms.
fn match_to_def(
  scrutinee: Name,
  arms: &[(Pattern, Term)],
  def_name: &Name,
  book: &mut MatchesBook,
  match_count: usize,
) -> Term {
  let free_vars: IndexSet<Name> = arms
    .iter()
    .flat_map(|(pat, term)| term.free_vars().into_keys().filter(|k| !pat.names().contains(k)))
    .collect();

  let mut rules: Vec<Rule> =
    arms.iter().map(|(pat, term)| Rule { pats: vec![pat.clone()], body: term.clone() }).collect();

  // Extend the rules with the free variables
  for rule in &mut rules {
    for var in &free_vars {
      rule.pats.push(Pattern::Var(Some(var.clone())));
    }
  }

  let new_name = make_def_name(def_name, &Name::new("match"), match_count);
  let def_id = book.def_names.insert(new_name);
  let def = Definition { def_id, generated: true, rules };
  book.new_defs.insert(def_id, def);

  let scrutinee_app = Term::App {
    tag: Tag::Static,
    fun: Box::new(Term::Ref { def_id }),
    arg: Box::new(Term::Var { nam: scrutinee }),
  };

  free_vars.into_iter().fold(scrutinee_app, |acc, nam| Term::App {
    tag: Tag::Static,
    fun: Box::new(acc),
    arg: Box::new(Term::Var { nam }),
  })
}

fn make_def_name(def_name: &Name, ctr: &Name, i: usize) -> Name {
  Name(format!("{def_name}${ctr}${i}"))
}

/// Finds the expected type of the matched argument.
/// Errors on incompatible types.
/// Short-circuits if the first pattern is Type::Any.
fn infer_match_type<'a>(
  pats: impl Iterator<Item = &'a Pattern>,
  ctrs: &HashMap<Name, Name>,
) -> Result<Type, MatchError> {
  let mut match_type = None;
  for pat in pats {
    let new_type = pat.to_type(ctrs).map_err(MatchError::Infer)?;
    match (new_type, &mut match_type) {
      (new, None) => match_type = Some(new),
      // TODO: Should we throw a type inference error in this case?
      // Since anything else will be sort of the wrong type (expected Var).
      (_, Some(Type::Any)) => (),
      (Type::Any, Some(_)) => (),
      (Type::Tup, Some(Type::Tup)) => (),
      (Type::Num, Some(Type::Num)) => (),
      (Type::Adt(nam_new), Some(Type::Adt(nam_old))) if &nam_new == nam_old => (),
      (new, Some(old)) => {
        return Err(MatchError::Infer(format!("Type mismatch. Found '{}' expected {}.", new, old)));
      }
    };
  }
  Ok(match_type.unwrap())
}

/// Transforms a match on Num with any possible patterns into 'match x {0: ...; +: @x-1 ...}'.
fn normalize_num_match(term: &mut Term) -> Result<(), MatchError> {
  let Term::Match { scrutinee: _, arms } = term else { unreachable!() };

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
        let body = Term::Lam {
          tag: Tag::Static,
          nam: Some(Name::new("%pred")),
          bod: Box::new(Term::Let {
            pat: Pattern::Var(Some(var.clone())),
            val: Box::new(Term::Opx {
              op: Op::ADD,
              fst: Box::new(Term::Var { nam: Name::new("%pred") }),
              snd: Box::new(Term::Num { val: 1 }),
            }),
            nxt: Box::new(std::mem::take(body)),
          }),
        };
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      // Var unused, so no need to increment
      // match x {0: ...; +: @* ...}
      Pattern::Var(None) => {
        let body = Term::Lam { tag: Tag::Static, nam: None, bod: Box::new(std::mem::take(body)) };
        succ_arm = Some((Pattern::Num(MatchNum::Succ(None)), body));
        break;
      }
      Pattern::Num(_) => (),
      _ => unreachable!(),
    }
  }

  let Some(zero_arm) = zero_arm else {
    return Err(MatchError::Missing(HashSet::from_iter([Name::new("0")])));
  };
  let Some(succ_arm) = succ_arm else {
    return Err(MatchError::Missing(HashSet::from_iter([Name::new("0")])));
  };
  *arms = vec![zero_arm, succ_arm];
  Ok(())
}
