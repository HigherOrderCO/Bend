use indexmap::IndexSet;
use itertools::Itertools;

use crate::{
  diagnostics::Info,
  term::{
    check::type_check::infer_match_arg_type,
    transform::encode_pattern_matching::{ExhaustivenessErr, MatchErr},
    Adts, Constructors, Ctx, Definition, Name, NumCtr, Pattern, Rule, Term, Type,
  },
};

impl Ctx<'_> {
  pub fn simplify_matches(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    for (def_name, def) in self.book.defs.iter_mut() {
      let res = def.simplify_matches(&self.book.ctrs, &self.book.adts);
      self.info.take_err(res, Some(def_name));
    }

    self.info.fatal(())
  }
}

impl Definition {
  pub fn simplify_matches(&mut self, ctrs: &Constructors, adts: &Adts) -> Result<(), MatchErr> {
    for rule in self.rules.iter_mut() {
      rule.body.simplify_matches(ctrs, adts)?;
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
  pub fn simplify_matches(&mut self, ctrs: &Constructors, adts: &Adts) -> Result<(), MatchErr> {
    match self {
      Term::Mat { args, rules } => {
        let (new_args, extracted) = extract_args(args);

        *self = simplify_match_expression(&new_args, rules, ctrs, adts)?;
        *self = bind_extracted_args(extracted, std::mem::take(self));
      }

      Term::Lst { els } => {
        for el in els {
          el.simplify_matches(ctrs, adts)?;
        }
      }
      Term::Let { val: fst, nxt: snd, .. }
      | Term::App { fun: fst, arg: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.simplify_matches(ctrs, adts)?;
        snd.simplify_matches(ctrs, adts)?;
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.simplify_matches(ctrs, adts)?,
      Term::Var { .. }
      | Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era => (),
      Term::Err => unreachable!(),
    }
    Ok(())
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
  args: &[Term],
  rules: &[Rule],
  ctrs: &Constructors,
  adts: &Adts,
) -> Result<Term, MatchErr> {
  let fst_row_irrefutable = rules[0].pats.iter().all(|p| p.is_wildcard());
  let fst_col_type = infer_match_arg_type(rules, 0, ctrs)?;

  if fst_row_irrefutable {
    irrefutable_fst_row_rule(args, rules, ctrs, adts)
  } else if fst_col_type == Type::Any {
    var_rule(args, rules, ctrs, adts)
  } else {
    switch_rule(args, rules, fst_col_type, ctrs, adts)
  }
}

/// Irrefutable row rule.
/// An optimization to not generate unnecessary pattern matching when we
/// know the first case always matches.
fn irrefutable_fst_row_rule(
  args: &[Term],
  rules: &[Rule],
  ctrs: &Constructors,
  adts: &Adts,
) -> Result<Term, MatchErr> {
  let mut term = rules[0].body.clone();
  term.simplify_matches(ctrs, adts)?;
  let term = rules[0].pats.iter().zip(args).fold(term, |term, (pat, arg)| Term::Let {
    pat: pat.clone(),
    val: Box::new(arg.clone()),
    nxt: Box::new(term),
  });
  Ok(term)
}

/// Var rule.
/// Could be merged with the ctr rule, but this is slightly simpler.
/// `match x0 ... xN { var p1 ... pN: (Body var p1 ... pN) }`
/// becomes
/// `match x1 ... xN { p1 ... pN: let var = x0; (Body var p1 ... pN) }`
fn var_rule(args: &[Term], rules: &[Rule], ctrs: &Constructors, adts: &Adts) -> Result<Term, MatchErr> {
  let mut new_rules = vec![];
  for rule in rules {
    let body = Term::Let {
      pat: rule.pats[0].clone(),
      val: Box::new(args[0].clone()),
      nxt: Box::new(rule.body.clone()),
    };
    let new_rule = Rule { pats: rule.pats[1 ..].to_vec(), body };
    new_rules.push(new_rule);
  }
  let mut term = Term::Mat { args: args[1 ..].to_vec(), rules: new_rules };
  term.simplify_matches(ctrs, adts)?;
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
  args: &[Term],
  rules: &[Rule],
  typ: Type,
  ctrs: &Constructors,
  adts: &Adts,
) -> Result<Term, MatchErr> {
  let mut new_rules = vec![];

  let adt_ctrs = match typ {
    Type::Num => {
      // Since numbers have infinite (2^60) constructors, they require special treatment.
      let mut ctrs = IndexSet::new();
      for rule in rules {
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
        return Err(MatchErr::NotExhaustive(ExhaustivenessErr(vec![Name::from("+")])));
      }
    }
    Type::NumSucc(exp) => {
      // Number match with + can't have number larger than that in the +
      // TODO: could be just a warning.
      for rule in rules {
        if let Pattern::Num(NumCtr::Num(got)) = rule.pats[0]
          && got >= exp
        {
          return Err(MatchErr::MalformedNumSucc(
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
    let nested_fields = switch_rule_nested_fields(arg_nam, &ctr);
    let matched_ctr = switch_rule_matched_ctr(ctr.clone(), &nested_fields);
    let mut body = switch_rule_submatch(args, rules, &matched_ctr, &nested_fields)?;
    body.simplify_matches(ctrs, adts)?;
    let pats = vec![matched_ctr];
    new_rules.push(Rule { pats, body });
  }
  let term = Term::Mat { args: vec![args[0].clone()], rules: new_rules };
  Ok(term)
}

fn switch_rule_nested_fields(arg_nam: &Name, ctr: &Pattern) -> Vec<Option<Name>> {
  let mut nested_fields = vec![];
  let old_vars = ctr.bind_or_eras();
  for old_var in old_vars {
    let new_nam = if let Some(field) = old_var {
      // Name of constructor field
      Name::new(format!("{arg_nam}%{field}"))
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
  ctr.bind_or_eras_mut().for_each(|var| *var = nested_fields.next().unwrap());
  ctr
}

fn switch_rule_submatch(
  args: &[Term],
  rules: &[Rule],
  ctr: &Pattern,
  nested_fields: &[Option<Name>],
) -> Result<Term, MatchErr> {
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
    return Err(MatchErr::NotExhaustive(ExhaustivenessErr(vec![ctr.ctr_name().unwrap()])));
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
    let nested_var_pats = nested_fields.iter().cloned().map(Pattern::Var);
    let old_pats = rule.pats[1 ..].iter().cloned();
    let pats = nested_var_pats.chain(old_pats).collect_vec();

    let body =
      Term::Let { pat: rule.pats[0].clone(), val: Box::new(ctr.to_term()), nxt: Box::new(rule.body.clone()) };

    Some(Rule { pats, body })
  } else {
    // Non-matching constructor. Don't include in submatch expression.
    None
  }
}

fn extract_args(args: &mut [Term]) -> (Vec<Term>, Vec<(Name, Term)>) {
  let mut new_args = vec![];
  let mut extracted = vec![];

  for (i, arg) in args.iter_mut().enumerate() {
    if matches!(arg, Term::Var { .. }) {
      new_args.push(std::mem::take(arg));
    } else {
      let nam = Name::new(format!("%match_arg{i}"));

      let old_arg = std::mem::take(arg);
      extracted.push((nam.clone(), old_arg));

      let new_arg = Term::Var { nam };
      new_args.push(new_arg);
    }
  }
  (new_args, extracted)
}

fn bind_extracted_args(extracted: Vec<(Name, Term)>, term: Term) -> Term {
  extracted.into_iter().rev().fold(term, |term, (nam, val)| Term::Let {
    pat: Pattern::Var(Some(nam)),
    val: Box::new(val),
    nxt: Box::new(term),
  })
}
