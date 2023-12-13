use crate::term::{
  check::type_check::{infer_arg_type, Type},
  display::DisplayJoin,
  Adt, Book, DefId, DefNames, Definition, Name, Pattern, Rule, Tag, Term,
};
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::{BTreeMap, HashMap, HashSet};

impl Book {
  /// Extracts any match terms into pattern matching functions.
  pub fn extract_matches(&mut self) -> Result<(), String> {
    let book = &mut MatchesBook::new(&mut self.def_names);

    for (def_id, def) in &mut self.defs {
      let def_name = book.def_names.name(def_id).cloned().unwrap();
      for rule in def.rules.iter_mut() {
        rule.body.extract_matches(&def_name, book, &mut 0).map_err(|err| err.to_string())?;
      }
    }

    if !book.new_defs.is_empty() {
      self.defs.append(&mut book.new_defs);
      self.flatten_rules();
    }

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
  pub fn check_matches<'book>(
    pats: &[Pattern],
    adts: &'book BTreeMap<Name, Adt>,
    ctrs: &'book HashMap<Name, Name>,
  ) -> Result<&'book Adt, MatchError> {
    let ty = infer_arg_type(pats.iter(), ctrs).map_err(MatchError::Infer)?;

    let Type::Adt(nam) = ty else { unreachable!() };

    let Adt { ctrs } = &adts[&nam];

    let mut missing: HashSet<_> = ctrs.keys().cloned().collect();

    for rule in pats {
      let Pattern::Ctr(nam, args) = rule else { unreachable!() };

      let mut binds = HashSet::new();

      for arg in args {
        for bind in arg.names() {
          if !binds.insert(bind) {
            return Err(MatchError::Repeated(bind.clone()));
          }
        }
      }

      missing.remove(nam);
    }

    if !missing.is_empty() {
      return Err(MatchError::Missing(missing));
    }

    Ok(&adts[&nam])
  }

  fn extract_matches(
    &mut self,
    def_name: &Name,
    book: &mut MatchesBook,
    match_count: &mut usize,
  ) -> Result<(), MatchError> {
    match self {
      Term::Match { scrutinee: box Term::Var { nam }, arms } => {
        if arms.is_empty() {
          return Err(MatchError::Empty);
        }

        for (_, term) in arms.iter_mut() {
          term.extract_matches(def_name, book, match_count)?;
        }

        *match_count += 1;

        let arms = std::mem::take(arms);
        let nam = std::mem::take(nam);

        *self = match_to_def(nam, &arms, def_name, book, match_count)?;
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.extract_matches(def_name, book, match_count)?;
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.extract_matches(def_name, book, match_count)?;
        snd.extract_matches(def_name, book, match_count)?;
      }
      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => {}

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
  match_count: &mut usize,
) -> Result<Term, MatchError> {
  let current_count = *match_count;

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

  let new_name = make_def_name(def_name, &Name::new("match"), current_count);
  let def_id = book.def_names.insert(new_name);
  let def = Definition { def_id, rules };
  book.new_defs.insert(def_id, def);

  let scrutinee_app = Term::App {
    tag: Tag::Static,
    fun: Box::new(Term::Ref { def_id }),
    arg: Box::new(Term::Var { nam: scrutinee }),
  };

  let app = free_vars.into_iter().fold(scrutinee_app, |acc, nam| Term::App {
    tag: Tag::Static,
    fun: Box::new(acc),
    arg: Box::new(Term::Var { nam }),
  });

  Ok(app)
}

fn make_def_name(def_name: &Name, ctr: &Name, i: usize) -> Name {
  Name(format!("{def_name}${ctr}${i}"))
}
