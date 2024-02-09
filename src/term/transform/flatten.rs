use std::collections::HashSet;

use crate::term::{Book, Definition, MatchNum, Name, Op, Origin, Pattern, Rule, Term};

impl Book {
  pub fn flatten_rules(&mut self) {
    let mut new_defs = vec![];
    for def in self.defs.values() {
      // Since these definitions don't need further processing it's fine to insert them all at the end.
      new_defs.append(&mut flatten_def(def));
    }
    for def in new_defs {
      self.defs.insert(def.name.clone(), def);
    }
  }
}

/// Splits a definition with nested rule patterns into a tree of definitions
/// with flat patterns, each matching a single layer of patterns.
fn flatten_def(def: &Definition) -> Vec<Definition> {
  let mut skip: HashSet<usize> = HashSet::new();
  let mut new_defs: Vec<Definition> = vec![];

  // We rebuild this definition rule by rule, with non-nested patterns
  let mut old_def = Definition { name: def.name.clone(), rules: vec![] };

  for i in 0 .. def.rules.len() {
    if skip.contains(&i) {
      continue;
    }

    let rule = &def.rules[i];
    let must_split = rule.pats.iter().any(|pat| !pat.is_flat());
    if must_split {
      // Create the entry for the new definition name
      let old_name = &def.name;
      let new_name = Name::from(format!("{}$F{}", old_name, new_defs.len()));

      // Create the rule that replaces the one being flattened.
      // Destructs one layer of the nested patterns and calls the following, forwarding the extracted fields.
      let old_rule = make_old_rule(rule, new_name.clone());
      old_def.rules.push(old_rule);

      // Create a new definition, with one rule for each rule that overlaps patterns with this one (including itself)
      // The rule patterns have one less layer of nesting and receive the destructed fields as extra args.
      let mut new_rules = vec![];
      for (j, other) in def.rules.iter().enumerate().skip(i) {
        let share_matches = rule.pats.iter().zip(&other.pats).all(|(a, b)| a.shares_matches_with(b));
        if share_matches {
          let new_rule = make_split_rule(rule, other);
          new_rules.push(new_rule);

          // Skip clauses that are already 100% covered by this one.
          // TODO: This is not enough to skip rules that are redundant but not a subset of any particular other rule.
          //   This means that it will sometimes generate redundant, unused rules.
          let other_is_subset = other.pats.iter().zip(&rule.pats).all(|(a, b)| a.is_flat_subset_of(b));
          if other_is_subset {
            skip.insert(j);
          }
        }
      }
      let new_def = Definition { name: new_name, rules: new_rules };
      // Recursively split the newly created def
      for def in flatten_def(&new_def) {
        new_defs.push(def);
      }
    } else {
      // If this rule is already flat, just mark it to be inserted back as it is.
      old_def.rules.push(def.rules[i].clone());
    }
  }
  new_defs.push(old_def);
  new_defs
}

/// Makes the rule that replaces the original.
/// The new version of the rule is flat and calls the next layer of pattern matching.
fn make_old_rule(rule: &Rule, split_def: Name) -> Rule {
  //(Foo Tic (Bar a b) (Haz c d)) = A
  //(Foo Tic x         y)         = B
  //---------------------------------
  //(Foo Tic (Bar a b) (Haz c d)) = B[x <- (Bar a b), y <- (Haz c d)]
  //
  //(Foo.0 a b c d) = ...
  let mut new_pats = Vec::new();
  let mut new_body_args = Vec::new();
  let mut var_count = 0;

  for arg in &rule.pats {
    match arg {
      Pattern::Ctr(arg_name, arg_args) => {
        let mut new_arg_args = Vec::new();
        for _ in arg_args {
          let var_name = make_var_name(&mut var_count);
          new_arg_args.push(Pattern::Var(Some(var_name.clone())));
          new_body_args.push(Term::Var { nam: var_name });
        }
        new_pats.push(Pattern::Ctr(arg_name.clone(), new_arg_args));
      }
      Pattern::Tup(_, _) => {
        let fst = make_var_name(&mut var_count);
        let snd = make_var_name(&mut var_count);
        new_body_args.push(Term::Var { nam: fst.clone() });
        new_body_args.push(Term::Var { nam: snd.clone() });
        let fst = Pattern::Var(Some(fst));
        let snd = Pattern::Var(Some(snd));
        new_pats.push(Pattern::Tup(Box::new(fst), Box::new(snd)));
      }
      Pattern::Var(_) => {
        let var_name = make_var_name(&mut var_count);
        new_body_args.push(Term::Var { nam: var_name.clone() });
        new_pats.push(Pattern::Var(Some(var_name)));
      }

      Pattern::Num(MatchNum::Zero) => {
        new_pats.push(Pattern::Num(MatchNum::Zero));
      }
      Pattern::Num(MatchNum::Succ(Some(_))) => {
        // Always flat, just pass on the variable
        let var_name = make_var_name(&mut var_count);
        new_body_args.push(Term::Var { nam: var_name.clone() });
        new_pats.push(Pattern::Num(MatchNum::Succ(Some(Some(var_name)))));
      }
      Pattern::Num(MatchNum::Succ(None)) => unreachable!(),
      Pattern::Lst(..) => unreachable!(),
    }
  }
  let new_body = Term::call(Term::Ref { nam: split_def }, new_body_args);
  Rule { pats: new_pats, body: new_body, origin: rule.origin }
}

/// Makes one of the new rules, flattening one layer of the original pattern.
fn make_split_rule(old_rule: &Rule, other_rule: &Rule) -> Rule {
  // (Foo a     (B x P) (C y0 y1)) = F
  // (Foo (A k) (B x Q) y        ) = G
  // -----------------------------
  // (Foo a (B x u) (C y0 y1)) = (Foo.0 a x u y0 y1)
  //   (Foo.0 a     x P y0 y1) = F
  //   (Foo.0 (A k) x Q f0 f1) = G [y <- (C f0 f1)] // f0 and f1 are fresh
  let mut new_pats = Vec::new();
  let mut new_body = other_rule.body.clone();
  let mut var_count = 0;
  for (rule_arg, other_arg) in old_rule.pats.iter().zip(&other_rule.pats) {
    match (rule_arg, other_arg) {
      // Two rules with same constructor
      (Pattern::Ctr(_, _), Pattern::Ctr(_, other_arg_args)) => {
        // We checked before that these two have the same constructor and match together
        for other_field in other_arg_args {
          new_pats.push(other_field.clone());
        }
      }
      (Pattern::Num(MatchNum::Zero), Pattern::Num(MatchNum::Zero)) => {
        // No internal fields, so nothing to forward
      }
      (Pattern::Num(MatchNum::Succ(_)), Pattern::Num(MatchNum::Succ(Some(nam)))) => {
        new_pats.push(Pattern::Var(nam.clone()))
      }
      (Pattern::Tup(_, _), Pattern::Tup(fst, snd)) => {
        new_pats.push(*fst.clone());
        new_pats.push(*snd.clone());
      }

      // First rule has constructor, second has used var
      (Pattern::Ctr(rule_arg_name, rule_arg_args), Pattern::Var(Some(other_arg))) => {
        let mut new_ctr_args = vec![];
        for _ in 0 .. rule_arg_args.len() {
          let new_nam = make_var_name(&mut var_count);
          new_ctr_args.push(Term::Var { nam: new_nam.clone() });
          new_pats.push(Pattern::Var(Some(new_nam)));
        }
        let def_ref = Term::r#ref(rule_arg_name);
        let new_ctr = Term::call(def_ref, new_ctr_args);
        new_body.subst(other_arg, &new_ctr);
      }
      (Pattern::Num(MatchNum::Zero), Pattern::Var(Some(other_arg))) => {
        let new_ctr = Term::Num { val: 0 };
        new_body.subst(other_arg, &new_ctr);
      }
      (Pattern::Num(MatchNum::Succ(Some(_))), Pattern::Var(Some(other_arg))) => {
        // Since the other rule used the entire number, we have to recover it after the decrementing.
        let new_nam = make_var_name(&mut var_count);
        let new_var = Term::Var { nam: new_nam.clone() };
        new_pats.push(Pattern::Var(Some(new_nam)));
        let new_ctr = Term::Opx { op: Op::ADD, fst: Box::new(new_var), snd: Box::new(Term::Num { val: 1 }) };
        new_body.subst(other_arg, &new_ctr);
      }
      (Pattern::Tup(..), Pattern::Var(Some(other_arg))) => {
        let fst_nam = make_var_name(&mut var_count);
        let snd_nam = make_var_name(&mut var_count);
        let fst_arg = Term::Var { nam: fst_nam.clone() };
        let snd_arg = Term::Var { nam: snd_nam.clone() };
        new_pats.push(Pattern::Var(Some(fst_nam)));
        new_pats.push(Pattern::Var(Some(snd_nam)));
        let new_ctr = Term::Tup { fst: Box::new(fst_arg), snd: Box::new(snd_arg) };
        new_body.subst(other_arg, &new_ctr);
      }

      // First rule has constructor, second has erased var.
      (Pattern::Ctr(_, ctr_fields), Pattern::Var(None)) => {
        for _ in ctr_fields {
          new_pats.push(Pattern::Var(None));
        }
      }
      (Pattern::Num(MatchNum::Zero), Pattern::Var(None)) => {
        // Nothing to pass forward
      }
      (Pattern::Num(MatchNum::Succ(_)), Pattern::Var(None)) => {
        new_pats.push(Pattern::Var(None));
      }
      (Pattern::Tup(..), Pattern::Var(None)) => {
        new_pats.push(Pattern::Var(None));
        new_pats.push(Pattern::Var(None));
      }

      // First rule has var.
      (Pattern::Var(_), _) => new_pats.push(other_arg.clone()),

      // Unreachable cases
      // We only call this function if we know the two patterns match together
      (
        Pattern::Ctr(..) | Pattern::Num(..) | Pattern::Tup(..),
        Pattern::Ctr(..) | Pattern::Num(..) | Pattern::Tup(..),
      ) => {
        unreachable!()
      }
      (Pattern::Num(MatchNum::Succ(None)), _) => unreachable!(
        "In pattern matching function position, number patterns can't have their lambda detached"
      ),
      (Pattern::Lst(..), _) | (_, Pattern::Lst(..)) => {
        unreachable!("List syntax should have been already desugared")
      }
    }
  }
  Rule { pats: new_pats, body: new_body, origin: Origin::Generated }
}

fn make_var_name(var_count: &mut usize) -> Name {
  let nam = format!("%x{var_count}");
  *var_count += 1;
  nam.into()
}
