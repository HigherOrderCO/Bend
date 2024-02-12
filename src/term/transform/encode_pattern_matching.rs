use crate::term::{
  check::type_check::DefinitionTypes, transform::unique_names::UniqueNameGenerator, AdtEncoding, Book,
  Definition, MatchNum, Name, Pattern, Rule, Tag, Term, Type,
};

impl Book {
  pub fn encode_pattern_matching_functions(
    &mut self,
    def_types: &DefinitionTypes,
    adt_encoding: AdtEncoding,
  ) {
    let def_names = self.defs.keys().cloned().collect::<Vec<_>>();
    for def_name in def_names {
      let def_type = &def_types[&def_name];

      let is_matching_def = def_type.iter().any(|t| matches!(t, Type::Adt(_) | Type::Tup | Type::Num));
      if is_matching_def {
        make_pattern_matching_def(self, &def_name, def_type, adt_encoding);
      } else {
        // For functions with only one rule that doesn't pattern match,
        // we just move the variables from arg to body.
        make_non_pattern_matching_def(self.defs.get_mut(&def_name).unwrap());
      }
    }
  }
}

/// For functions that don't pattern match, just move the arg variables into the body.
fn make_non_pattern_matching_def(def: &mut Definition) {
  let rule = def.rules.first_mut().unwrap();
  let body = add_non_match_arg_lams(std::mem::take(&mut rule.body), &rule.pats);
  def.rules = vec![Rule { pats: vec![], body }];
}

/// For functions that do pattern match,
///  we break them into a tree of small matching functions
///  with the original rule bodies at the end.
fn make_pattern_matching_def(book: &mut Book, def_name: &Name, def_type: &[Type], adt_encoding: AdtEncoding) {
  // First push the pattern bound vars into the rule body
  let rules = &mut book.defs.get_mut(def_name).unwrap().rules;
  for rule in rules.iter_mut() {
    rule.body = add_non_match_arg_lams(std::mem::take(&mut rule.body), &rule.pats);
  }

  // Generate scott-encoded pattern matching
  let def = &book.defs[def_name];
  let crnt_rules = (0 .. def.rules.len()).collect();
  let mut new_body = make_pattern_matching_case(book, def, def_type, crnt_rules, vec![], adt_encoding);

  // Simplify the generated term
  new_body.eta_reduction();
  // We have to give unique names to avoid name conflicts when beta-reducing.
  // Ex: @x (@y @x (y x) x) => @x @x (x x)
  let mut name_gen = UniqueNameGenerator::default();
  name_gen.unique_names_in_term(&mut new_body);
  beta_reduce(&mut new_body);

  // Substitute the rule bodies into the generated term
  for (rule_idx, rule) in def.rules.iter().enumerate() {
    // TODO: We should generate this things normalized to avoid this mess.
    let subst_var = rule_body_subst_var(rule_idx);
    subst_rule_body(&mut new_body, &subst_var, &rule.body, &mut name_gen);
  }
  // This is already a mess, so I'm putting this here so that we can at least read the result if it bugs.
  UniqueNameGenerator::default().unique_names_in_term(&mut new_body);

  // Put the new body back into the definition.
  let def = book.defs.get_mut(def_name).unwrap();
  def.rules = vec![Rule { pats: vec![], body: new_body }];
}

/// Builds the encoding for the patterns in a pattern matching function.
fn make_pattern_matching_case(
  book: &Book,
  def: &Definition,
  def_type: &[Type],
  crnt_rules: Vec<usize>,
  match_path: Vec<Pattern>,
  adt_encoding: AdtEncoding,
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

  if is_fst_rule_irrefutable {
    // First rule will always be selected, generate leaf case.
    make_leaf_case(book, fst_rule, fst_rule_idx, match_path, adt_encoding)
  } else {
    make_match_case(book, def, def_type, crnt_rules, match_path, adt_encoding)
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
  adt_encoding: AdtEncoding,
) -> Term {
  let next_arg_idx = match_path.len();
  let next_type = &def_type[next_arg_idx];

  // Create the subfunctions
  let mut next_cases = vec![];
  let next_ctrs = if next_type.is_var_type() {
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
    let next_case = make_pattern_matching_case(book, def, def_type, next_rules, match_path, adt_encoding);
    next_cases.push(next_case);
  }

  let (old_args, new_args) = args_from_match_path(&match_path);

  // Encode the current pattern matching, calling the subfunctions
  let match_var = Name::new("x");
  // The match term itself
  let term = encode_match(next_type, &match_var, next_cases.into_iter(), adt_encoding);
  // The calls to the args of previous matches
  let term = old_args.iter().chain(new_args.iter()).cloned().fold(term, Term::arg_call);
  // Lambda for pattern matched value
  let term = Term::named_lam(match_var, term);
  // The bindings of the previous args
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book, adt_encoding);
  term
}

fn encode_match(
  match_type: &Type,
  match_var: &Name,
  mut arms: impl Iterator<Item = Term>,
  adt_encoding: AdtEncoding,
) -> Term {
  match match_type {
    Type::None => unreachable!(),
    // (arm[0] x)
    Type::Any => Term::arg_call(arms.next().unwrap(), match_var.clone()),
    // let (%fst, %snd) = x; (arm[0] %fst %snd)
    Type::Tup => Term::Let {
      pat: Pattern::Tup(
        Box::new(Pattern::Var(Some(Name::new("%fst")))),
        Box::new(Pattern::Var(Some(Name::new("%snd")))),
      ),
      val: Box::new(Term::Var { nam: match_var.clone() }),
      nxt: Box::new(Term::call(arms.next().unwrap(), [Term::Var { nam: Name::new("%fst") }, Term::Var {
        nam: Name::new("%snd"),
      }])),
    },
    // match x {0: arm[0]; +: arm[1]}
    Type::Num => Term::Mat {
      matched: Box::new(Term::Var { nam: match_var.clone() }),
      arms: vec![
        (Pattern::Num(MatchNum::Zero), arms.next().unwrap()),
        (Pattern::Num(MatchNum::Succ(None)), arms.next().unwrap()),
      ],
    },
    Type::Adt(adt_name) => match adt_encoding {
      // (x arm[0] arm[1] ...)
      AdtEncoding::Scott => Term::call(Term::Var { nam: match_var.clone() }, arms),
      // #adt_name(x arm[0] arm[1] ...)
      AdtEncoding::TaggedScott => {
        Term::tagged_call(Tag::adt_name(adt_name), Term::Var { nam: match_var.clone() }, arms)
      }
    },
  }
}

/// Builds the function calling one of the original rule bodies.
fn make_leaf_case(
  book: &Book,
  rule: &Rule,
  rule_idx: usize,
  match_path: Vec<Pattern>,
  adt_encoding: AdtEncoding,
) -> Term {
  let (old_args, new_args) = args_from_match_path(&match_path);
  let args = &mut old_args.iter().chain(new_args.iter()).cloned();

  // The term we're building
  // Instead of directly pasting the rule body here (or a ref to the rule body),
  // we instead put a variable that we will substitute later with the correct term.
  // We do this to normalize the generated term but not the rule body.
  // TODO: Generate the pattern matching Term already in reduced form to avoid this jank.
  let term = Term::Var { nam: rule_body_subst_var(rule_idx) };

  // Add the applications to call the rule body
  let term = match_path.iter().zip(&rule.pats).fold(term, |term, (matched, pat)| {
    if matched.flat_equals(pat) {
      matched.vars().fold(term, |term, _| Term::arg_call(term, args.next().unwrap()))
    } else {
      // The pattern in this rule was a Var, but we matched on a constructor.
      // Rebuild the constructor
      let mut ctr = matched.clone();
      ctr.vars_mut().for_each(|var| *var = Some(args.next().unwrap()));
      Term::app(term, ctr.to_term())
    }
  });
  // Add the lambdas for the matched args.
  let term = add_arg_lams(term, old_args, new_args, match_path.last(), book, adt_encoding);

  term
}

/// Adds the argument lambdas to the term, without considering pattern matching.
/// Adds a lambda for each variable in the rule patterns.
fn add_non_match_arg_lams(body: Term, pats: &[Pattern]) -> Term {
  let vars = pats.iter().flat_map(|p| p.vars().cloned());
  vars.rev().fold(body, |bod, var| Term::lam(var, bod))
}

/// Adds the argument lambdas to the term, with new args followed by old args.
fn add_arg_lams(
  term: Term,
  old_args: Vec<Name>,
  new_args: Vec<Name>,
  last_pat: Option<&Pattern>,
  book: &Book,
  adt_encoding: AdtEncoding,
) -> Term {
  // Add lams for old vars
  let term = old_args.into_iter().rev().fold(term, |term, arg| Term::named_lam(arg, term));

  // Add lams for new vars, with named tags for Ctr fields
  match last_pat {
    // New vars from Ctr
    Some(Pattern::Ctr(ctr, args)) => {
      new_args.into_iter().rev().zip(args.iter().rev()).fold(term, |term, (new_arg, pat)| {
        let adt = &book.ctrs[ctr];
        let Pattern::Var(Some(field)) = pat else { unreachable!() };
        match adt_encoding {
          AdtEncoding::Scott => Term::Lam { tag: Tag::Static, nam: Some(new_arg), bod: Box::new(term) },
          AdtEncoding::TaggedScott => {
            Term::Lam { tag: Tag::adt_field(adt, ctr, field), nam: Some(new_arg), bod: Box::new(term) }
          }
        }
      })
    }
    // New vars from other pats
    Some(_) => new_args.into_iter().rev().fold(term, |term, new_arg| Term::named_lam(new_arg, term)),
    // First arg, no new vars.
    None => term,
  }
}

fn args_from_match_path(match_path: &[Pattern]) -> (Vec<Name>, Vec<Name>) {
  let (new_match, old_matches) = match_path.split_last().unzip();
  let old_args: Vec<Name> = old_matches
    .unwrap_or_default()
    .iter()
    .flat_map(|pat| pat.vars())
    .enumerate()
    .map(|(i, _)| format!("%x{i}").into())
    .collect();
  let new_args: Vec<Name> = new_match
    .map(|pat| pat.vars().enumerate().map(|(i, _)| format!("%y{i}").into()).collect())
    .unwrap_or(vec![]);
  (old_args, new_args)
}

/* Functions used to normalize generated part of the def */

/// Name for a variable to be substituted with the rule body.
fn rule_body_subst_var(rule_idx: usize) -> Name {
  format!("%rule_subst_{rule_idx}").into()
}

/// Beta reduces a term.
/// Assumes a term in the specific format generated by the pass in this file (linear, unique names, at most one let expr in a row).
fn beta_reduce(term: &mut Term) {
  // TODO: Very dumb, slow.
  while normal_order_step(term) {}
}

/// Tries one reduction in normal order and returns whether something was reduced.
fn normal_order_step(term: &mut Term) -> bool {
  match term {
    Term::App { tag: app_tag, fun, arg } => {
      match fun.as_mut() {
        // #tag (#tag @x bod arg) -> bod/[x -> arg]
        // #tag (#tag @* bod arg) -> bod
        Term::Lam { tag: lam_tag, nam, bod } if app_tag == lam_tag => {
          if let Some(nam) = nam {
            bod.subst(nam, arg.as_ref());
          }
          *term = std::mem::take(bod);
          true
        }
        // Reduce through let expressions.
        // #tag ( let pat = val; #tag @x bod arg) -> let pat = val; bod/[x -> arg]
        Term::Let { pat, val, nxt: box Term::Lam { tag: lam_tag, nam, bod } } if app_tag == lam_tag => {
          if let Some(nam) = nam {
            bod.subst(nam, arg.as_ref());
          }
          *term = Term::Let {
            pat: std::mem::replace(pat, Pattern::Var(None)),
            val: std::mem::take(val),
            nxt: std::mem::take(bod),
          };
          true
        }
        _ => normal_order_step(fun) || normal_order_step(arg),
      }
    }
    Term::Mat { matched: scrutinee, arms } => {
      if normal_order_step(scrutinee) {
        return true;
      }
      for (_, arm) in arms {
        if normal_order_step(arm) {
          return true;
        }
      }
      false
    }
    Term::Lst { els } => {
      for el in els {
        if normal_order_step(el) {
          return true;
        }
      }
      false
    }
    Term::Let { val: fst, nxt: snd, .. }
    | Term::Tup { fst, snd }
    | Term::Dup { val: fst, nxt: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Opx { fst, snd, .. } => normal_order_step(fst) || normal_order_step(snd),
    Term::Lam { bod, .. } | Term::Chn { bod, .. } => normal_order_step(bod),
    Term::Var { .. }
    | Term::Lnk { .. }
    | Term::Num { .. }
    | Term::Str { .. }
    | Term::Ref { .. }
    | Term::Era
    | Term::Err => false,
  }
}

/// Substitute one of the rule bodies, while making sure that the generated part is normalized.
///
/// Example:
///   Assume this pattern matching function:
///     `(foo (a, b)) = <Rule body>`
///   If we substituted the rule body directly onto the generated pattern matching term, we'd get:
///     `(foo) = λx let (%fst, %snd) = x; (λa λb <Rule body> %fst %snd)`
///   Instead, we want to generate this:
///     `(foo) = λx let (%fst, %snd) = x; <Rule body>`
fn subst_rule_body(term: &mut Term, subst_var: &Name, body: &Term, name_gen: &mut UniqueNameGenerator) {
  fn leading_apps(term: &mut Term) -> (&mut Term, usize) {
    if let Term::App { tag: _, fun, arg: _ } = term {
      let (term, n_apps) = leading_apps(fun);
      (term, n_apps + 1)
    } else {
      (term, 0)
    }
  }

  match term {
    term @ Term::App { .. } => {
      // Reduce the surrounding applications.
      // They should never be more than the lambdas for receiving arguments.
      let (subst_term, n_apps) = leading_apps(term);
      if let Term::Var { nam } = subst_term
        && nam == subst_var
      {
        // Subst and reduce the surrounding application
        subst_term.subst(subst_var, body);
        // So that we don't have name conflicts with the generated term.
        name_gen.unique_names_in_term(subst_term);
        for _ in 0 .. n_apps {
          // The arg applications will always be the leftmost outermost.
          let reduced = normal_order_step(term);
          debug_assert!(reduced);
        }
      } else {
        // Normal App, not a rule body substitution
        let Term::App { tag: _, fun, arg } = term else { unreachable!() }; // to appease the borrow checker
        subst_rule_body(fun, subst_var, body, name_gen);
        subst_rule_body(arg, subst_var, body, name_gen);
      }
    }
    // Rule body not applied to anything, just subst
    Term::Var { nam } if nam == subst_var => {
      term.subst(subst_var, body);
    }

    Term::Mat { matched: scrutinee, arms } => {
      subst_rule_body(scrutinee, subst_var, body, name_gen);
      for (_, arm) in arms {
        subst_rule_body(arm, subst_var, body, name_gen);
      }
    }
    Term::Lst { els } => {
      for el in els {
        subst_rule_body(el, subst_var, body, name_gen);
      }
    }
    Term::Let { val: fst, nxt: snd, .. }
    | Term::Tup { fst, snd }
    | Term::Dup { val: fst, nxt: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Opx { fst, snd, .. } => {
      subst_rule_body(fst, subst_var, body, name_gen);
      subst_rule_body(snd, subst_var, body, name_gen);
    }
    Term::Lam { bod, .. } | Term::Chn { bod, .. } => subst_rule_body(bod, subst_var, body, name_gen),
    Term::Var { .. }
    | Term::Lnk { .. }
    | Term::Num { .. }
    | Term::Str { .. }
    | Term::Ref { .. }
    | Term::Era
    | Term::Err => (),
  }
}
