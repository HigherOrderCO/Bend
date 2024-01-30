use crate::term::{
  check::type_check::DefinitionTypes, Book, DefId, MatchNum, Name, Pattern, Rule, Tag, Term, Type,
};

impl Book {
  pub fn encode_pattern_matching_functions(&mut self, def_types: &DefinitionTypes) {
    for def_id in self.defs.keys().copied().collect::<Vec<_>>() {
      let def_type = &def_types[&def_id];

      let is_matching_def = def_type.iter().any(|t| matches!(t, Type::Adt(_) | Type::Tup | Type::Num));
      if is_matching_def {
        make_pattern_matching_def(self, def_id, def_type);
      } else {
        // For functions with only one rule that doesn't pattern match,
        // we just move the variables from arg to body.
        make_non_pattern_matching_def(self, def_id);
      }
    }
  }
}

/// For functions that don't pattern match, just move the arg variables into the body.
fn make_non_pattern_matching_def(book: &mut Book, def_id: DefId) {
  let def = book.defs.get_mut(&def_id).unwrap();
  let mut rule = std::mem::take(def.rules.first_mut().unwrap());
  for pat in rule.pats.iter().rev() {
    let Pattern::Var(var) = pat else { unreachable!() };
    let bod = std::mem::take(&mut rule.body);
    rule.body = Term::Lam { tag: Tag::Static, nam: var.clone(), bod: Box::new(bod) };
  }
  rule.pats = vec![];
  def.rules = vec![rule];
}

/// For functions that do pattern match,
///  we break them into a tree of small matching functions
///  with the original rule bodies at the end.
fn make_pattern_matching_def(book: &mut Book, def_id: DefId, def_type: &[Type]) {
  let def_name = book.def_names.name(&def_id).unwrap().clone();
  let def = book.defs.get_mut(&def_id).unwrap();
  let origin = def.rules[0].origin;
  let crnt_rules = (0 .. def.rules.len()).collect();

  // First create a definition for each rule body
  let mut rule_bodies = vec![];
  for rule in def.rules.iter_mut() {
    let body = std::mem::take(&mut rule.body);
    let body = make_rule_body(body, &rule.pats);
    rule_bodies.push(body);
  }
  for (rule_idx, body) in rule_bodies.into_iter().enumerate() {
    let rule_name = make_rule_name(&def_name, rule_idx);
    book.insert_def(rule_name, vec![Rule { pats: vec![], body, origin }]);
  }

  // Generate scott-encoded pattern matching
  let new_body = make_pattern_matching_case(book, def_type, def_id, crnt_rules, vec![]);
  let def = book.defs.get_mut(&def_id).unwrap();
  def.rules = vec![Rule { pats: vec![], body: new_body, origin }];
}

fn make_rule_name(def_name: &Name, rule_idx: usize) -> Name {
  Name(format!("{def_name}$R{rule_idx}"))
}

fn make_rule_body(mut body: Term, pats: &[Pattern]) -> Term {
  // Add the lambdas for the pattern variables
  for pat in pats.iter().rev() {
    match pat {
      Pattern::Var(nam) => body = Term::Lam { tag: Tag::Static, nam: nam.clone(), bod: Box::new(body) },
      Pattern::Ctr(_, vars) => {
        for var in vars.iter().rev() {
          let Pattern::Var(nam) = var else { unreachable!() };
          body = Term::Lam { tag: Tag::Static, nam: nam.clone(), bod: Box::new(body) }
        }
      }
      Pattern::Num(MatchNum::Zero) => (),
      Pattern::Num(MatchNum::Succ(None)) => (),
      Pattern::Num(MatchNum::Succ(Some(nam))) => {
        body = Term::Lam { tag: Tag::Static, nam: nam.clone(), bod: Box::new(body) }
      }
      pat @ Pattern::Tup(..) => {
        let nam = Name::new("%0");
        body = Term::named_lam(nam.clone(), Term::Let {
          pat: pat.clone(),
          val: Box::new(Term::Var { nam }),
          nxt: Box::new(body),
        });
      }
      Pattern::List(..) => unreachable!(),
    }
  }
  body
}

/// Builds the encoding for the patterns in a pattern matching function.
fn make_pattern_matching_case(
  book: &Book,
  def_type: &[Type],
  def_id: DefId,
  crnt_rules: Vec<usize>,
  match_path: Vec<Pattern>,
) -> Term {
  let def = &book.defs[&def_id];
  // This is safe since we check exhaustiveness earlier.
  let fst_rule_idx = crnt_rules[0];
  let fst_rule = &def.rules[fst_rule_idx];
  let crnt_arg_idx = match_path.len();

  // Check if we've reached the end for this subfunction.
  // We did if all the (possibly zero) remaining patterns are variables and not matches.
  let all_args_done = crnt_arg_idx >= def.arity();
  let only_var_left = fst_rule.pats[crnt_arg_idx ..].iter().all(|p| matches!(p, Pattern::Var(_)));
  let is_fst_rule_irrefutable = all_args_done || only_var_left;

  let (new_match, old_matches) = match_path.split_last().unzip();

  let old_args: Vec<Name> = old_matches
    .unwrap_or_default()
    .iter()
    .flat_map(|pat| pat.vars())
    .enumerate()
    .map(|(i, _)| Name(format!("%x_{i}")))
    .collect();
  let new_args: Vec<Name> = new_match
    .map(|pat| pat.vars().enumerate().map(|(i, _)| Name(format!("%y_{i}"))).collect())
    .unwrap_or(vec![]);

  if is_fst_rule_irrefutable {
    // First rule will always be selected, generate leaf case.
    make_leaf_case(book, def_id, fst_rule_idx, match_path, old_args, new_args)
  } else {
    make_match_case(book, def_type, def_id, crnt_rules, match_path, old_args, new_args)
  }
}

fn make_match_case(
  book: &Book,
  def_type: &[Type],
  def_id: DefId,
  crnt_rules: Vec<usize>,
  match_path: Vec<Pattern>,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
) -> Term {
  let next_arg_idx = match_path.len();
  let next_type = &def_type[next_arg_idx];

  // Create the subfunctions
  let mut next_cases = vec![];
  let next_ctrs = if matches!(next_type, Type::Any) {
    vec![Pattern::Var(Some(Name::new("x")))]
  } else {
    next_type.ctrs(&book.adts)
  };
  for pat in &next_ctrs {
    let def = &book.defs[&def_id];
    let next_rules = crnt_rules
      .iter()
      .copied()
      .filter(|&rule_idx| pat.is_flat_subset_of(&def.rules[rule_idx].pats[next_arg_idx]))
      .collect();
    let mut match_path = match_path.clone();
    match_path.push(pat.clone());
    let next_case = make_pattern_matching_case(book, def_type, def_id, next_rules, match_path);
    next_cases.push(next_case);
  }

  // Encode the current pattern matching, calling the subfunctions
  let match_var = Name::new("x");
  // The match term itself
  let term = match next_type {
    Type::None => unreachable!(),
    Type::Any => Term::arg_call(std::mem::take(&mut next_cases[0]), Some(match_var.clone())),
    Type::Tup => Term::Let {
      pat: Pattern::Tup(
        Box::new(Pattern::Var(Some(Name::new("%fst")))),
        Box::new(Pattern::Var(Some(Name::new("%snd")))),
      ),
      val: Box::new(Term::Var { nam: match_var.clone() }),
      nxt: Box::new(Term::call(std::mem::take(&mut next_cases[0]), [
        Term::Var { nam: Name::new("%fst") },
        Term::Var { nam: Name::new("%snd") },
      ])),
    },
    Type::Num => Term::Match {
      scrutinee: Box::new(Term::Var { nam: Name::new("x") }),
      arms: vec![
        (Pattern::Num(MatchNum::Zero), std::mem::take(&mut next_cases[0])),
        (Pattern::Num(MatchNum::Succ(None)), std::mem::take(&mut next_cases[1])),
      ],
    },
    Type::Adt(adt_name) => {
      Term::tagged_call(Term::Var { nam: match_var.clone() }, next_cases, Tag::Named(adt_name.clone()))
    }
  };
  // The calls to the args of previous matches
  let term = add_arg_calls(term, old_args.clone(), new_args.clone());
  // Lambda for pattern matched value
  let term = Term::named_lam(match_var, term);
  // The bindings of the previous args
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book);
  term
}

/// Builds the function calling one of the original rule bodies.
fn make_leaf_case(
  book: &Book,
  def_id: DefId,
  rule_idx: usize,
  match_path: Vec<Pattern>,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
) -> Term {
  let def_name = book.def_names.name(&def_id).unwrap();
  let rule_def_name = make_rule_name(def_name, rule_idx);
  let rule_def_id = book.def_names.def_id(&rule_def_name).unwrap();
  let rule = &book.defs[&def_id].rules[rule_idx];

  let args = &mut old_args.iter().chain(new_args.iter()).cloned();

  // The term we're building
  let term = Term::Ref { def_id: rule_def_id };
  // Add the applications to call the rule body
  let term = match_path.iter().zip(&rule.pats).fold(term, |term, (matched, pat)| {
    if matched.flat_equals(pat) {
      matched.vars().fold(term, |term, _| Term::arg_call(term, Some(args.next().unwrap())))
    } else {
      // This particular rule was not matching on this arg but due to the other rules we had to match on a constructor.
      // So, to call the rule body we have to recreate the constructor.
      // (On scott encoding, if one of the cases is matched we must also match on all the other constructors for this arg)
      matched.to_term(&book.def_names)
    }
  });
  // Add the lambdas for the matched args.
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book);

  term
}

/// Adds the argument calls to the term, with old args followed by new args.
fn add_arg_calls(term: Term, old_args: Vec<Name>, new_args: Vec<Name>) -> Term {
  old_args.into_iter().chain(new_args).fold(term, |term, arg| Term::arg_call(term, Some(arg)))
}

// Adds the argument lambdas to the term, with new args followed by old args.
fn add_arg_lams(
  term: Term,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
  last_pat: Option<&Pattern>,
  book: &Book,
) -> Term {
  let term = old_args.into_iter().rev().fold(term, |term, arg| Term::named_lam(arg, term));

  // Add lambdas for the new vars, with tags depending on the matched pattern.
  if let Some(last_pat) = last_pat {
    match last_pat {
      Pattern::Ctr(ctr, args) => {
        new_args.into_iter().rev().zip(args.iter().rev()).fold(term, |term, (new_arg, pat)| {
          let adt = &book.ctrs[ctr];
          let Pattern::Var(Some(field)) = pat else { unreachable!() };
          let tag = Tag::Named(Name(format!("{adt}.{ctr}.{field}")));
          Term::tagged_lam(tag, new_arg, term)
        })
      }
      _ => new_args.into_iter().fold(term, |term, new_arg| Term::named_lam(new_arg, term)),
    }
  } else {
    term
  }
}
