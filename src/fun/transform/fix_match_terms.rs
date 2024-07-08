use crate::{
  diagnostics::{Diagnostics, WarningType, ERR_INDENT_SIZE},
  fun::{Adts, Constructors, CtrField, Ctx, MatchRule, Name, Num, Term},
  maybe_grow,
};
use std::collections::HashMap;

enum FixMatchErr {
  AdtMismatch { expected: Name, found: Name, ctr: Name },
  NonExhaustiveMatch { typ: Name, missing: Name },
  IrrefutableMatch { var: Option<Name> },
  UnreachableMatchArms { var: Option<Name> },
  RedundantArm { ctr: Name },
}

impl Ctx<'_> {
  /// Convert all match and switch expressions to a normalized form.
  /// * For matches, resolve the constructors and create the name of the field variables.
  /// * For switches, resolve the succ case ("_") and create the name of the pred variable.
  /// * If the match arg is not a variable, it is separated into a let expression and bound to "%matched"
  /// * Check for redundant arms and non-exhaustive matches.
  ///
  /// Example:
  /// For the program
  /// ```hvm
  /// data MyList = (Cons h t) | Nil
  /// match x {
  ///   Cons: (A x.h x.t)
  ///   Nil: switch (Foo y) { 0: B; 1: C; _: D }
  /// }
  /// ```
  /// The following AST transformations will be made:
  /// * The binds `x.h` and `x.t` will be generated and stored in the match term.
  /// * `(Foo y)`` will be put in a let expression, bound to the variable `%matched`;
  /// * The bind `%matched-2` will be generated and stored in the switch term.
  /// * If either was missing one of the match cases (a number or a constructor), we'd get an error.
  /// * If either included one of the cases more than once (including wildcard patterns), we'd get a warning.
  /// ```hvm
  /// match x {
  ///   Cons: (A x.h x.t)
  ///   Nil: let %matched = (Foo y); switch %matched { 0: B; 1: C; _: D }
  /// }
  /// ```
  pub fn fix_match_terms(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for def in self.book.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        let errs = rule.body.fix_match_terms(&self.book.ctrs, &self.book.adts);

        for err in errs {
          match err {
            FixMatchErr::AdtMismatch { .. } | FixMatchErr::NonExhaustiveMatch { .. } => {
              self.info.add_rule_error(err, def.name.clone())
            }
            FixMatchErr::IrrefutableMatch { .. } => {
              self.info.add_rule_warning(err, WarningType::IrrefutableMatch, def.name.clone())
            }
            FixMatchErr::UnreachableMatchArms { .. } => {
              self.info.add_rule_warning(err, WarningType::UnreachableMatch, def.name.clone())
            }
            FixMatchErr::RedundantArm { .. } => {
              self.info.add_rule_warning(err, WarningType::RedundantMatch, def.name.clone())
            }
          }
        }
      }
    }

    self.info.fatal(())
  }
}

impl Term {
  fn fix_match_terms(&mut self, ctrs: &Constructors, adts: &Adts) -> Vec<FixMatchErr> {
    maybe_grow(|| {
      let mut errs = Vec::new();

      for child in self.children_mut() {
        let mut e = child.fix_match_terms(ctrs, adts);
        errs.append(&mut e);
      }

      if matches!(self, Term::Mat { .. } | Term::Fold { .. }) {
        self.fix_match(&mut errs, ctrs, adts);
      }
      match self {
        Term::Def { def, nxt } => {
          for rule in def.rules.iter_mut() {
            errs.extend(rule.body.fix_match_terms(ctrs, adts));
          }
          errs.extend(nxt.fix_match_terms(ctrs, adts));
        }
        // Add a use term to each arm rebuilding the matched variable
        Term::Mat { arg: _, bnd, with_bnd: _, with_arg: _, arms }
        | Term::Fold { bnd, arg: _, with_bnd: _, with_arg: _, arms } => {
          for (ctr, fields, body) in arms {
            if let Some(ctr) = ctr {
              *body = Term::Use {
                nam: bnd.clone(),
                val: Box::new(Term::call(
                  Term::Ref { nam: ctr.clone() },
                  fields.iter().flatten().cloned().map(|nam| Term::Var { nam }),
                )),
                nxt: Box::new(std::mem::take(body)),
              };
            }
          }
        }
        Term::Swt { arg: _, bnd, with_bnd: _, with_arg: _, pred, arms } => {
          let n_nums = arms.len() - 1;
          for (i, arm) in arms.iter_mut().enumerate() {
            let orig = if i == n_nums {
              Term::add_num(Term::Var { nam: pred.clone().unwrap() }, Num::U24(i as u32))
            } else {
              Term::Num { val: Num::U24(i as u32) }
            };
            *arm = Term::Use { nam: bnd.clone(), val: Box::new(orig), nxt: Box::new(std::mem::take(arm)) };
          }
        }
        _ => {}
      }

      // Remove the bound name
      match self {
        Term::Mat { bnd, .. } | Term::Swt { bnd, .. } | Term::Fold { bnd, .. } => *bnd = None,
        _ => {}
      }

      errs
    })
  }

  fn fix_match(&mut self, errs: &mut Vec<FixMatchErr>, ctrs: &Constructors, adts: &Adts) {
    let (Term::Mat { bnd, arg, with_bnd, with_arg, arms }
    | Term::Fold { bnd, arg, with_bnd, with_arg, arms }) = self
    else {
      unreachable!()
    };
    let bnd = bnd.clone().unwrap();

    // Normalize arms, making one arm for each constructor of the matched adt.
    if let Some(ctr_nam) = &arms[0].0 {
      if let Some(adt_nam) = ctrs.get(ctr_nam) {
        // First arm matches a constructor as expected, so we can normalize the arms.
        let adt_ctrs = &adts[adt_nam].ctrs;

        // Decide which constructor corresponds to which arm of the match.
        let mut bodies = fixed_match_arms(&bnd, arms, adt_nam, adt_ctrs.keys(), ctrs, adts, errs);

        // Build the match arms, with all constructors
        let mut new_rules = vec![];
        for (ctr, fields) in adt_ctrs.iter() {
          let fields = fields.iter().map(|f| Some(match_field(&bnd, &f.nam))).collect::<Vec<_>>();
          let body = if let Some(Some(body)) = bodies.remove(ctr) {
            body
          } else {
            errs.push(FixMatchErr::NonExhaustiveMatch { typ: adt_nam.clone(), missing: ctr.clone() });
            Term::Err
          };
          new_rules.push((Some(ctr.clone()), fields, body));
        }
        *arms = new_rules;
        return;
      }
    }

    // First arm was not matching a constructor, irrefutable match, convert into a use term.
    errs.push(FixMatchErr::IrrefutableMatch { var: arms[0].0.clone() });
    let match_var = arms[0].0.take();
    let arg = std::mem::take(arg);
    let with_bnd = std::mem::take(with_bnd);
    let with_arg = std::mem::take(with_arg);

    // Replaces `self` by its irrefutable arm
    *self = std::mem::take(&mut arms[0].2);

    // `with` clause desugaring
    // Performs the same as `Term::linearize_match_with`.
    // Note that it only wraps the arm with function calls if `with_bnd` and `with_arg` aren't empty.
    *self = Term::rfold_lams(std::mem::take(self), with_bnd.into_iter());
    *self = Term::call(std::mem::take(self), with_arg);

    if let Some(var) = match_var {
      *self = Term::Use {
        nam: Some(bnd.clone()),
        val: arg,
        nxt: Box::new(Term::Use {
          nam: Some(var),
          val: Box::new(Term::Var { nam: bnd }),
          nxt: Box::new(std::mem::take(self)),
        }),
      }
    }
  }
}

/// Given the rules of a match term, return the bodies that match
/// each of the constructors of the matched ADT.
///
/// If no rules match a certain constructor, return None in the map,
/// indicating a non-exhaustive match.
fn fixed_match_arms<'a>(
  bnd: &Name,
  rules: &mut Vec<MatchRule>,
  adt_nam: &Name,
  adt_ctrs: impl Iterator<Item = &'a Name>,
  ctrs: &Constructors,
  adts: &Adts,
  errs: &mut Vec<FixMatchErr>,
) -> HashMap<&'a Name, Option<Term>> {
  let mut bodies = HashMap::<&Name, Option<Term>>::from_iter(adt_ctrs.map(|ctr| (ctr, None)));
  for rule_idx in 0..rules.len() {
    // If Ctr arm, use the body of this rule for this constructor.
    if let Some(ctr_nam) = &rules[rule_idx].0 {
      if let Some(found_adt) = ctrs.get(ctr_nam) {
        if found_adt == adt_nam {
          let body = bodies.get_mut(ctr_nam).unwrap();
          if body.is_none() {
            // Use this rule for this constructor
            *body = Some(rules[rule_idx].2.clone());
          } else {
            errs.push(FixMatchErr::RedundantArm { ctr: ctr_nam.clone() });
          }
        } else {
          errs.push(FixMatchErr::AdtMismatch {
            expected: adt_nam.clone(),
            found: found_adt.clone(),
            ctr: ctr_nam.clone(),
          })
        }
        continue;
      }
    }
    // Otherwise, Var arm, use the body of this rule for all non-covered constructors.
    for (ctr, body) in bodies.iter_mut() {
      if body.is_none() {
        let mut new_body = rules[rule_idx].2.clone();
        if let Some(var) = &rules[rule_idx].0 {
          new_body = Term::Use {
            nam: Some(var.clone()),
            val: Box::new(rebuild_ctr(bnd, ctr, &adts[adt_nam].ctrs[&**ctr])),
            nxt: Box::new(new_body),
          };
        }
        *body = Some(new_body);
      }
    }
    if rule_idx != rules.len() - 1 {
      errs.push(FixMatchErr::UnreachableMatchArms { var: rules[rule_idx].0.clone() });
      rules.truncate(rule_idx + 1);
    }
    break;
  }

  bodies
}

fn match_field(arg: &Name, field: &Name) -> Name {
  Name::new(format!("{arg}.{field}"))
}

fn rebuild_ctr(arg: &Name, ctr: &Name, fields: &[CtrField]) -> Term {
  let ctr = Term::Ref { nam: ctr.clone() };
  let fields = fields.iter().map(|f| Term::Var { nam: match_field(arg, &f.nam) });
  Term::call(ctr, fields)
}

impl std::fmt::Display for FixMatchErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      FixMatchErr::AdtMismatch { expected, found, ctr } => write!(
        f,
        "Type mismatch in 'match' expression: Expected a constructor of type '{expected}', found '{ctr}' of type '{found}'"
      ),
      FixMatchErr::NonExhaustiveMatch { typ, missing } => {
        write!(f, "Non-exhaustive 'match' expression of type '{typ}'. Case '{missing}' not covered.")
      }
      FixMatchErr::IrrefutableMatch { var } => {
        writeln!(
          f,
          "Irrefutable 'match' expression. All cases after variable pattern '{}' will be ignored.",
          var.as_ref().unwrap_or(&Name::new("*")),
        )?;
        writeln!(
          f,
          "{:ERR_INDENT_SIZE$}Note that to use a 'match' expression, the matched constructors need to be defined in a 'data' definition.",
          "",
        )?;
        write!(
          f,
          "{:ERR_INDENT_SIZE$}If this is not a mistake, consider using a 'let' expression instead.",
          ""
        )
      }

      FixMatchErr::UnreachableMatchArms { var } => write!(
        f,
        "Unreachable arms in 'match' expression. All cases after '{}' will be ignored.",
        var.as_ref().unwrap_or(&Name::new("*"))
      ),
      FixMatchErr::RedundantArm { ctr } => {
        write!(f, "Redundant arm in 'match' expression. Case '{ctr}' appears more than once.")
      }
    }
  }
}
