use crate::{
  diagnostics::{Diagnostics, ToStringVerbose, ERR_INDENT_SIZE},
  term::{
    check::type_check::{infer_match_arg_type, TypeMismatchErr},
    display::{DisplayFn, DisplayJoin},
    Adts, Constructors, Ctx, Definition, Name, NumCtr, Pattern, Rule, Term, Type,
  },
};
use indexmap::IndexSet;
use itertools::Itertools;

pub enum SimplifyMatchErr {
  NotExhaustive(Vec<Name>),
  TypeMismatch(TypeMismatchErr),
  MalformedNumSucc(Pattern, Pattern),
}

impl Ctx<'_> {
  pub fn simplify_matches(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      let res = def.simplify_matches(&self.book.ctrs, &self.book.adts);
      self.info.take_rule_err(res, def_name.clone());
    }

    self.info.fatal(())
  }
}

impl Definition {
  pub fn simplify_matches(&mut self, ctrs: &Constructors, adts: &Adts) -> Result<(), SimplifyMatchErr> {
    let name_gen = &mut 0;
    for rule in self.rules.iter_mut() {
      rule.body.simplify_matches(ctrs, adts, name_gen)?;
    }
    Ok(())
  }
}

impl Term {
  /// Converts match expressions with multiple matched arguments and
  /// arbitrary patterns into matches on a single value, with only
  /// simple (non-nested) patterns, and one rule for each constructor.
  ///
  /// See `[simplify_match_expression]` for more information.
  pub fn simplify_matches(
    &mut self,
    ctrs: &Constructors,
    adts: &Adts,
    name_gen: &mut usize,
  ) -> Result<(), SimplifyMatchErr> {
    Term::recursive_call(move || {
      match self {
        Term::Mat { args, rules } => {
          let extracted = extract_args(args);
          let args = std::mem::take(args);
          let rules = std::mem::take(rules);
          let term = simplify_match_expression(args, rules, ctrs, adts, name_gen)?;
          *self = bind_extracted_args(extracted, term);
        }

        _ => {
          for child in self.children_mut() {
            child.simplify_matches(ctrs, adts, name_gen)?;
          }
        }
      }
      Ok(())
    })
  }
}

/// Given the values held by a match expression, separates a simple, non-nested
/// match out of the first argument of the given match.
///
/// If there are constructors of different types, returns a type error.
///
/// If no patterns match one of the constructors, returns a non-exhaustive match error.
///
/// Invariant: All the match arg terms are variables.
///
/// ===========================================================================
///
/// For each constructor, a match case is created. It has only a single simple
/// pattern with the constructor and no subpatterns.
///
/// The match cases follow the same order as the order the constructors are in
/// the ADT declaration.
///
/// Any nested subpatterns are extracted and moved into a nested match
/// expression, together with the remaining match arguments.
///
/// For Var matches, we skip creating the surrounding match term since it's
/// redundant. (would be simply match x {x: ...})
fn simplify_match_expression(
  args: Vec<Term>,
  rules: Vec<Rule>,
  ctrs: &Constructors,
  adts: &Adts,
  name_gen: &mut usize,
) -> Result<Term, SimplifyMatchErr> {
  let fst_row_irrefutable = rules[0].pats.iter().all(|p| p.is_wildcard());
  let fst_col_type = infer_match_arg_type(&rules, 0, ctrs)?;

  if fst_row_irrefutable {
    irrefutable_fst_row_rule(args, rules, ctrs, adts, name_gen)
  } else if fst_col_type == Type::Any {
    var_rule(args, rules, ctrs, adts, name_gen)
  } else {
    switch_rule(args, rules, fst_col_type, ctrs, adts, name_gen)
  }
}

/// Irrefutable row rule.
/// An optimization to not generate unnecessary pattern matching when we
/// know the first case always matches.
fn irrefutable_fst_row_rule(
  args: Vec<Term>,
  mut rules: Vec<Rule>,
  ctrs: &Constructors,
  adts: &Adts,
  name_gen: &mut usize,
) -> Result<Term, SimplifyMatchErr> {
  rules.truncate(1);

  let Rule { pats, body: mut term } = rules.pop().unwrap();
  term.simplify_matches(ctrs, adts, name_gen)?;

  for (pat, arg) in pats.iter().zip(args.iter()) {
    for bind in pat.binds().flatten() {
      term.subst(bind, arg);
    }
  }

  Ok(term)
}

/// Var rule.
/// Could be merged with the ctr rule, but this is slightly simpler.
/// `match x0 ... xN { var p1 ... pN: (Body var p1 ... pN) }`
/// becomes
/// `match x1 ... xN { p1 ... pN: let var = x0; (Body var p1 ... pN) }`
fn var_rule(
  mut args: Vec<Term>,
  rules: Vec<Rule>,
  ctrs: &Constructors,
  adts: &Adts,
  name_gen: &mut usize,
) -> Result<Term, SimplifyMatchErr> {
  let mut new_rules = vec![];
  for mut rule in rules {
    let rest = rule.pats.split_off(1);

    let pat = rule.pats.pop().unwrap();
    let mut body = rule.body;
    if let Pattern::Var(Some(nam)) = &pat {
      body.subst(nam, &args[0]);
    }

    let new_rule = Rule { pats: rest, body };
    new_rules.push(new_rule);
  }

  let rest = args.split_off(1);
  let mut term = Term::Mat { args: rest, rules: new_rules };
  term.simplify_matches(ctrs, adts, name_gen)?;
  Ok(term)
}

/// When the first column has constructors, create a branch on the constructors
/// of the first match arg.
///
/// Each created match case has another nested match
/// expression to deal with the extracted nested patterns and remaining args.
///
/// ```hvm
/// match x0 ... xN {
///   (CtrA p0_0_0 ... p0_A) p0_1 ... p0_N : (Body0 p0_0_0 ... p0_0_A p0_1 ... p0_N)
///   ...
///   varI pI_1 ... pI_N: (BodyI varI pI_1 ... pI_N)
///   ...
///   (CtrB pJ_0_0 ... pJ_0_B) pJ_1 ... pJ_N: (BodyJ pJ_0_0 ... pJ_0_B pJ_1 ... pJ_N)
///   ...
///   (CtrC) pK_1 ... pK_N: (BodyK p_1 ... pK_N)
///   ...
/// }
/// ```
/// is converted into
/// ```hvm
/// match x0 {
///   (CtrA x0%ctrA_field0 ... x%ctrA_fieldA): match x%ctrA_field0 ... x%ctrA_fieldN x1 ... xN {
///     p0_0_0 ... p0_0_B p0_1 ... p0_N :
///       (Body0 p0_0_0 ... p0_0_B )
///     x%ctrA_field0 ... x%ctrA_fieldA pI_1 ... pI_N:
///       let varI = (CtrA x%ctrA_field0 ... x%ctrA_fieldN); (BodyI varI pI_1 ... pI_N)
///     ...
///   }
///   (CtrB x%ctrB_field0 ... x%ctrB_fieldB): match x%ctrB_field0 ... x%ctrB_fieldB x1 ... xN {
///     x%ctrB_field0 ... x%ctrB_fieldB pI_1 ... pI_N:
///       let varI = (CtrB x%ctrB_field0 ... x%ctrB_fieldB); (BodyI varI pI_1 ... pI_N)
///     pJ_0_0 ... pJ_0_B pJ_1 ... pJ_N :
///       (BodyJ pJ_0_0 ... pJ_0_B pJ_1 ... pJ_N)
///     ...
///   }
///   (CtrC): match * x1 ... xN {
///     * pI_1 ... pI_N:
///       let varI = CtrC; (BodyI varI pI_1 ... pI_N)
///     * pK_1 ... pK_N:
///       (BodyK p_1 ... pK_N)
///     ...
///   }
///   ...
/// }
/// ```
fn switch_rule(
  mut args: Vec<Term>,
  rules: Vec<Rule>,
  typ: Type,
  ctrs: &Constructors,
  adts: &Adts,
  name_gen: &mut usize,
) -> Result<Term, SimplifyMatchErr> {
  let mut new_rules = vec![];

  let adt_ctrs = match typ {
    Type::Num => {
      // Since numbers have infinite (2^60) constructors, they require special treatment.
      let mut ctrs = IndexSet::new();
      for rule in &rules {
        ctrs.insert(rule.pats[0].clone());
        if rule.pats[0].is_wildcard() {
          break;
        }
      }

      Vec::from_iter(ctrs)
    }
    _ => typ.ctrs(adts),
  };

  // Check valid number match
  match typ {
    Type::Num => {
      // Number match without + must have a default case
      if !rules.iter().any(|r| r.pats[0].is_wildcard()) {
        return Err(SimplifyMatchErr::NotExhaustive(vec![Name::from("+")]));
      }
    }
    Type::NumSucc(exp) => {
      // Number match with + can't have number larger than that in the +
      // TODO: could be just a warning.
      for rule in &rules {
        if let Pattern::Num(NumCtr::Num(got)) = rule.pats[0]
          && got >= exp
        {
          return Err(SimplifyMatchErr::MalformedNumSucc(
            rule.pats[0].clone(),
            Pattern::Num(NumCtr::Succ(exp, None)),
          ));
        }
      }
    }
    _ => (),
  }

  for ctr in adt_ctrs {
    // Create the matched constructor and the name of the bound variables.
    let Term::Var { nam: arg_nam } = &args[0] else { unreachable!() };
    let nested_fields = switch_rule_nested_fields(arg_nam, &ctr, name_gen);
    let matched_ctr = switch_rule_matched_ctr(ctr.clone(), &nested_fields);
    let mut body = switch_rule_submatch(&args, &rules, &matched_ctr, &nested_fields)?;
    body.simplify_matches(ctrs, adts, name_gen)?;
    let pats = vec![matched_ctr];
    new_rules.push(Rule { pats, body });
  }
  args.truncate(1);
  let term = Term::Mat { args, rules: new_rules };
  Ok(term)
}

fn switch_rule_nested_fields(arg_nam: &Name, ctr: &Pattern, name_gen: &mut usize) -> Vec<Option<Name>> {
  let mut nested_fields = vec![];
  let old_vars = ctr.binds();
  for old_var in old_vars {
    *name_gen += 1;
    let new_nam = if let Some(field) = old_var {
      // Name of constructor field
      Name::new(format!("{arg_nam}%{field}%{name_gen}"))
    } else {
      // Name of var pattern
      arg_nam.clone()
    };
    nested_fields.push(Some(new_nam));
  }
  if nested_fields.is_empty() {
    // We say that a unit variant always has an unused wildcard nested
    nested_fields.push(None);
  }
  nested_fields
}

fn switch_rule_matched_ctr(mut ctr: Pattern, nested_fields: &[Option<Name>]) -> Pattern {
  let mut nested_fields = nested_fields.iter().cloned();
  ctr.binds_mut().for_each(|var| *var = nested_fields.next().unwrap());
  ctr
}

fn switch_rule_submatch(
  args: &[Term],
  rules: &[Rule],
  ctr: &Pattern,
  nested_fields: &[Option<Name>],
) -> Result<Term, SimplifyMatchErr> {
  // Create the nested match expression.
  let new_args = nested_fields.iter().cloned().map(Term::var_or_era);
  let old_args = args[1 ..].iter().cloned();
  let args = new_args.clone().chain(old_args).collect::<Vec<_>>();

  // Make the match cases of the nested submatch, filtering out the rules
  // that don't match the current ctr.
  let rules =
    rules.iter().filter_map(|rule| switch_rule_submatch_arm(rule, ctr, nested_fields)).collect::<Vec<_>>();

  if rules.is_empty() {
    // TODO: Return the full pattern
    return Err(SimplifyMatchErr::NotExhaustive(vec![ctr.ctr_name().unwrap()]));
  }

  let mat = Term::Mat { args, rules };
  Ok(mat)
}

/// Make one of the cases of the nested submatch of a switch case.
/// Returns None if the rule doesn't match the constructor.
fn switch_rule_submatch_arm(rule: &Rule, ctr: &Pattern, nested_fields: &[Option<Name>]) -> Option<Rule> {
  if rule.pats[0].simple_equals(ctr) {
    // Same ctr, extract the patterns.
    // match x ... {(Ctr p0_0...) ...: Body; ...}
    // becomes
    // match x {(Ctr x%field0 ...): match x1 ... {p0_0 ...: Body; ...}; ...}
    let mut pats = match &rule.pats[0] {
      // Since the variable in the succ ctr is not a nested pattern, we need this special case.
      Pattern::Num(NumCtr::Succ(_, Some(var))) => vec![Pattern::Var(var.clone())],
      // Similarly for the default case in a num match
      Pattern::Var(var) => vec![Pattern::Var(var.clone())],
      _ => rule.pats[0].children().cloned().collect::<Vec<_>>(),
    };
    if pats.is_empty() {
      // We say that a unit variant always has an unused wildcard nested
      pats.push(Pattern::Var(None));
    }
    pats.extend(rule.pats[1 ..].iter().cloned());
    let body = rule.body.clone();
    Some(Rule { pats, body })
  } else if rule.pats[0].is_wildcard() {
    // Var, reconstruct the value matched in the expression above.
    // match x ... {var ...: Body; ...}
    // becomes
    // match x {
    //   (Ctr x%field0 ...): match x1 ... {
    //     x%field0 ...: let var = (Ctr x%field0 ...); Body;
    //   ... };
    // ... }
    let mut body = rule.body.clone();
    if let Pattern::Var(Some(nam)) = &rule.pats[0] {
      body.subst(nam, &ctr.to_term());
    }

    let nested_var_pats = nested_fields.iter().cloned().map(Pattern::Var);
    let old_pats = rule.pats[1 ..].iter().cloned();
    let pats = nested_var_pats.chain(old_pats).collect_vec();

    Some(Rule { pats, body })
  } else {
    // Non-matching constructor. Don't include in submatch expression.
    None
  }
}

/// Swaps non-Var arguments in a match by vars with generated names,
/// returning a vec with the extracted args and what they were replaced by.
///
/// `match Term {...}` => `match %match_arg0 {...}` + `vec![(%match_arg0, Term)])`
fn extract_args(args: &mut [Term]) -> Vec<(Name, Term)> {
  let mut extracted = vec![];

  for (i, arg) in args.iter_mut().enumerate() {
    if !matches!(arg, Term::Var { .. }) {
      let nam = Name::new(format!("%match_arg{i}"));
      let new_arg = Term::Var { nam: nam.clone() };
      let old_arg = std::mem::replace(arg, new_arg);
      extracted.push((nam, old_arg));
    }
  }
  extracted
}

/// Binds the arguments that were extracted from a match with [`extract_args`];
///
/// `vec![(%match_arg0, arg)]` + `term` => `let %match_arg0 = arg; term`
fn bind_extracted_args(extracted: Vec<(Name, Term)>, term: Term) -> Term {
  extracted.into_iter().rfold(term, |term, (nam, val)| Term::Let {
    pat: Pattern::Var(Some(nam)),
    val: Box::new(val),
    nxt: Box::new(term),
  })
}

const PATTERN_ERROR_LIMIT: usize = 5;
const ERROR_LIMIT_HINT: &str = "Use the --verbose option to see all cases.";

impl SimplifyMatchErr {
  pub fn display(&self, verbose: bool) -> impl std::fmt::Display + '_ {
    let limit = if verbose { usize::MAX } else { PATTERN_ERROR_LIMIT };
    DisplayFn(move |f| match self {
      SimplifyMatchErr::NotExhaustive(cases) => {
        let ident = ERR_INDENT_SIZE * 2;
        let hints = DisplayJoin(
          || {
            cases
              .iter()
              .take(limit)
              .map(|pat| DisplayFn(move |f| write!(f, "{:ident$}Case '{pat}' not covered.", "")))
          },
          "\n",
        );
        write!(f, "Non-exhaustive pattern matching. Hint:\n{}", hints)?;

        let len = cases.len();
        if len > limit {
          write!(f, " ... and {} others.\n{:ident$}{}", len - limit, "", ERROR_LIMIT_HINT)?;
        }

        Ok(())
      }
      SimplifyMatchErr::TypeMismatch(err) => {
        write!(f, "{}", err.to_string_verbose(verbose))
      }
      SimplifyMatchErr::MalformedNumSucc(got, exp) => {
        write!(f, "Expected a sequence of incrementing numbers ending with '{exp}', found '{got}'.")
      }
    })
  }
}

impl ToStringVerbose for SimplifyMatchErr {
  fn to_string_verbose(&self, verbose: bool) -> String {
    format!("{}", self.display(verbose))
  }
}

impl From<TypeMismatchErr> for SimplifyMatchErr {
  fn from(value: TypeMismatchErr) -> Self {
    SimplifyMatchErr::TypeMismatch(value)
  }
}
