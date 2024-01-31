use crate::term::{
  check::type_check::DefinitionTypes, Book, DefId, Definition, MatchNum, Name, Pattern, Rule, Tag, Term, Type,
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
  let rule = def.rules.first_mut().unwrap();
  let vars = rule.pats.iter().map(|p| p.vars().cloned()).flatten();
  let body = std::mem::take(&mut rule.body);
  let body = vars.fold(body, |bod, var| Term::lam(var, bod));
  def.rules = vec![Rule { pats: vec![], body, origin: rule.origin }];
}

/// For functions that do pattern match,
///  we break them into a tree of small matching functions
///  with the original rule bodies at the end.
fn make_pattern_matching_def(book: &mut Book, def_id: DefId, def_type: &[Type]) {
  // First push the pattern bound vars into the rule body
  let rules = &mut book.defs.get_mut(&def_id).unwrap().rules;
  for rule in rules {
    let vars = rule.pats.iter().map(|p| p.vars().cloned()).flatten();
    let body = std::mem::take(&mut rule.body);
    let body = vars.rev().fold(body, |bod, var| Term::lam(var, bod));
    rule.body = body;
  }

  // Generate scott-encoded pattern matching
  let def = &book.defs[&def_id];
  let crnt_rules = (0 .. def.rules.len()).collect();
  let mut new_body = make_pattern_matching_case(book, def, def_type, crnt_rules, vec![]);

  // Simplify the generated term
  new_body.eta_reduction();
  beta_reduce(&mut new_body);

  // Substitute the rule bodies into the generated term
  // TODO: Substituting it here prevents beta-reducing the rule body args.
  // Ex: `(foo (a, b)) = (a, b)` => `(foo) = λx let (%fst, %snd) = x; (λa λb (a, b) %fst %snd)`
  // Notice the generated `(λa λb (a, b) %fst %snd)`
  for (rule_idx, rule) in def.rules.iter().enumerate() {
    new_body.subst(&rule_body_subst_var(rule_idx), &rule.body);
  }

  // Put the new body back into the definition.
  let def = book.defs.get_mut(&def_id).unwrap();
  def.rules = vec![Rule { pats: vec![], body: new_body, origin }];
}

/// Builds the encoding for the patterns in a pattern matching function.
fn make_pattern_matching_case(
  book: &Book,
  def: &Definition,
  def_type: &[Type],
  crnt_rules: Vec<usize>,
  match_path: Vec<Pattern>,
) -> Term {
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
    .map(|(i, _)| Name(format!("%x{i}")))
    .collect();
  let new_args: Vec<Name> = new_match
    .map(|pat| pat.vars().enumerate().map(|(i, _)| Name(format!("%y{i}"))).collect())
    .unwrap_or(vec![]);

  if is_fst_rule_irrefutable {
    // First rule will always be selected, generate leaf case.
    make_leaf_case(book, fst_rule, fst_rule_idx, match_path, old_args, new_args)
  } else {
    make_match_case(book, def, def_type, crnt_rules, match_path, old_args, new_args)
  }
}

/// Builds the term that pattern matches on the current arg.
/// Recursively builds the terms matching the next args.
fn make_match_case(
  book: &Book,
  def: &Definition,
  def_type: &[Type],
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
    let next_rules = crnt_rules
      .iter()
      .copied()
      .filter(|&rule_idx| pat.is_flat_subset_of(&def.rules[rule_idx].pats[next_arg_idx]))
      .collect();
    let mut match_path = match_path.clone();
    match_path.push(pat.clone());
    let next_case = make_pattern_matching_case(book, def, def_type, next_rules, match_path);
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
  let term = old_args.iter().chain(new_args.iter()).cloned().fold(term, Term::arg_call);
  // Lambda for pattern matched value
  let term = Term::lam(Some(match_var), term);
  // The bindings of the previous args
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book);
  term
}

/// Builds the function calling one of the original rule bodies.
fn make_leaf_case(
  book: &Book,
  rule: &Rule,
  rule_idx: usize,
  match_path: Vec<Pattern>,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
) -> Term {
  let args = &mut old_args.iter().chain(new_args.iter()).cloned();

  // The term we're building
  // Instead of directly pasting the rule body here (or a ref to the rule body),
  // we instead put a variable that we will substitute later with the correct term.
  // We do this to run simplifying passes on the generated term (eta-reduction and pre-reduction).
  // TODO: Generate the pattern matching Term already in reduced form to avoid this jank.
  let term = Term::Var { nam: rule_body_subst_var(rule_idx) };

  // Add the applications to call the rule body
  let term = match_path.iter().zip(&rule.pats).fold(term, |term, (matched, pat)| {
    if matched.flat_equals(pat) {
      matched.vars().fold(term, |term, _| Term::arg_call(term, Some(args.next().unwrap())))
    } else {
      // The pattern in this rule was a Var, but we matched on a constructor.
      // Rebuild the constructor
      Term::call(term, [matched.to_term(&book.def_names)])
    }
  });
  // Add the lambdas for the matched args.
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book);

  term
}

/// Adds the argument lambdas to the term, with new args followed by old args.
fn add_arg_lams(
  term: Term,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
  last_pat: Option<&Pattern>,
  book: &Book,
) -> Term {
  // Add lams for old vars
  let term = old_args.into_iter().rev().fold(term, |term, arg| Term::lam(Some(arg), term));

  // Add lams for new vars
  if let Some(last_pat) = last_pat {
    match last_pat {
      // Ctr patterns use a named tag for the fields
      Pattern::Ctr(ctr, args) => {
        new_args.into_iter().rev().zip(args.iter().rev()).fold(term, |term, (new_arg, pat)| {
          let adt = &book.ctrs[ctr];
          let Pattern::Var(Some(field)) = pat else { unreachable!() };
          let tag = Tag::Named(Name(format!("{adt}.{ctr}.{field}")));
          Term::tagged_lam(tag, new_arg, term)
        })
      }
      _ => new_args.into_iter().rev().fold(term, |term, new_arg| Term::lam(Some(new_arg), term)),
    }
  } else {
    term
  }
}

/// Name for a variable to be substituted with the rule body.
fn rule_body_subst_var(rule_idx: usize) -> Name {
  Name(format!("%rule_subst_{rule_idx}"))
}

// Beta reduces a term.
// Assumes a term in the specific format generated by the pass in this file.
fn beta_reduce(term: &mut Term) {
  match term {
    Term::App { tag: app_tag, fun, arg } => {
      // #tag (#tag @x bod arg) -> bod/[x -> arg]
      // #tag (#tag @* bod arg) -> bod
      if let Term::Lam { tag: lam_tag, nam, bod } = fun.as_mut() {
        if app_tag == lam_tag {
          if let Some(nam) = nam {
            bod.subst(nam, arg.as_ref());
          }
          *term = std::mem::take(bod);
          beta_reduce(term);
        } else {
          beta_reduce(fun);
          beta_reduce(arg);
        }
      } else {
        beta_reduce(fun);
        beta_reduce(arg);
      }
    }
    Term::Match { scrutinee, arms } => {
      beta_reduce(scrutinee);
      for (_, arm) in arms {
        beta_reduce(arm);
      }
    }
    Term::List { els } => {
      for el in els {
        beta_reduce(el);
      }
    }
    Term::Let { val: fst, nxt: snd, .. }
    | Term::Tup { fst, snd }
    | Term::Dup { val: fst, nxt: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Opx { fst, snd, .. } => {
      beta_reduce(fst);
      beta_reduce(snd);
    }
    Term::Lam { bod, .. } | Term::Chn { bod, .. } => beta_reduce(bod),
    Term::Var { .. }
    | Term::Lnk { .. }
    | Term::Num { .. }
    | Term::Str { .. }
    | Term::Ref { .. }
    | Term::Era => (),
  }
}
