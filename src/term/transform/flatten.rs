use crate::term::{Book, DefId, DefNames, Definition, MatchNum, Name, Origin, Pattern, Rule, Term};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

impl Book {
  pub fn flatten_rules(&mut self) {
    for def_id in self.defs.keys().copied().collect_vec() {
      let new_defs = flatten_def(&self.defs[&def_id], &mut self.def_names);
      for def in new_defs {
        self.defs.insert(def.def_id, def);
      }
    }
  }
}

fn flatten_def(def: &Definition, def_names: &mut DefNames) -> Vec<Definition> {
  // Groups rules by function name
  let def_name = def_names.name(&def.def_id).unwrap().clone();

  // For each group, split its internal rules
  let new_rules = split_def(&def_name, &def.rules, def_names);

  new_rules
    .into_iter()
    .map(|(name, rules)| Definition { def_id: def_names.def_id(&name).unwrap(), rules })
    .collect()
}

impl Pattern {
  /// True if the two non-nested patterns cover some overlapping cases.
  fn overlaps_with(&self, other: &Pattern) -> bool {
    match (self, other) {
      (Pattern::Ctr(a_nam, a_args), Pattern::Ctr(b_nam, b_args))
        if a_nam == b_nam && a_args.len() == b_args.len() =>
      {
        true
      }
      (Pattern::Num(MatchNum::Zero), Pattern::Num(MatchNum::Zero)) => true,
      (Pattern::Num(MatchNum::Succ(_)), Pattern::Num(MatchNum::Succ(_))) => true,
      (Pattern::Tup(..), Pattern::Tup(..)) => true,
      (Pattern::Var(..), _) => true,
      (_, Pattern::Var(..)) => true,
      (Pattern::List(..), _) | (_, Pattern::List(..)) => unreachable!(),
      _ => false,
    }
  }

  /// True if when a term matches `other` it always also matches `self`.
  fn is_superset_of(&self, other: &Pattern) -> bool {
    match (self, other) {
      (Pattern::Ctr(a_nam, a_args), Pattern::Ctr(b_nam, b_args))
        if a_nam == b_nam && a_args.len() == b_args.len() =>
      {
        true
      }
      (Pattern::Num(MatchNum::Zero), Pattern::Num(MatchNum::Zero)) => true,
      (Pattern::Num(MatchNum::Succ(_)), Pattern::Num(MatchNum::Succ(_))) => true,
      (Pattern::Tup(..), Pattern::Tup(..)) => true,
      (Pattern::Var(..), _) => true,
      (Pattern::List(..), _) | (_, Pattern::List(..)) => unreachable!(),
      _ => false,
    }
  }
}

fn split_def(name: &Name, rules: &[Rule], def_names: &mut DefNames) -> Vec<(Name, Vec<Rule>)> {
  let mut skip: HashSet<usize> = HashSet::new();
  let mut new_defs: HashMap<Name, Vec<Rule>> = HashMap::new();
  let mut split_rule_count = 0;
  for i in 0 .. rules.len() {
    if skip.contains(&i) {
      continue;
    }

    let rule = &rules[i];
    let must_split = rule.pats.iter().any(|pat| !pat.is_flat());
    if must_split {
      let new_split_name = Name(format!("{}$F{}", name, split_rule_count));
      split_rule_count += 1;
      let new_split_def_id = def_names.insert(new_split_name.clone());

      // Create the rule that replaces the one being flattened.
      // Destructs one layer of the nested patterns and calls the following, forwarding the extracted fields.
      let old_rule = make_old_rule(rule, new_split_def_id);
      new_defs.entry(name.clone()).or_default().push(old_rule);

      // Create a new definition, with one rule for each rule that overlaps patterns with this one (including itself)
      // The rule patterns have one less layer of nesting and receive the destructed fields as extra args.
      let mut new_rules = vec![];
      for (j, other) in rules.iter().enumerate().skip(i) {
        let has_overlap = rule.pats.iter().zip(&other.pats).all(|(a, b)| a.overlaps_with(b));
        if has_overlap {
          let new_rule = make_split_rule(rule, other, def_names);
          new_rules.push(new_rule);

          // Skip clauses that are already 100% covered by this one.
          // TODO: This is not enough to skip rules that are redundant but not a subset of any particular other rule.
          //   This means that it will sometimes generate redundant, unused rules.
          let is_superset = rule.pats.iter().zip(&other.pats).all(|(a, b)| a.is_superset_of(b));
          if is_superset {
            skip.insert(j);
          }
        }
      }
      // Recursively split the newly created def
      for (nam, mut rules) in split_def(&new_split_name, &new_rules, def_names) {
        new_defs.entry(nam).or_default().append(&mut rules);
      }
    } else {
      // If this rule is already flat, just mark it to be inserted back as it is.
      new_defs.entry(name.clone()).or_default().push(rules[i].clone());
    }
  }
  new_defs.into_iter().collect()
}

/// Makes the rule that replaces the original.
/// The new version of the rule is flat and calls the next layer of pattern matching.
fn make_old_rule(rule: &Rule, new_split_def_id: DefId) -> Rule {
  //(Foo Tic (Bar a b) (Haz c d)) = A
  //(Foo Tic x         y)         = B
  //---------------------------------
  //(Foo Tic (Bar a b) (Haz c d)) = B[x <- (Bar a b), y <- (Haz c d)]
  //
  //(Foo.0 a b c d) = ...
  let pats = &rule.pats;
  let mut new_pats = Vec::new();
  let mut new_body_args = Vec::new();
  let mut var_count = 0;

  for arg in pats {
    match arg {
      Pattern::Ctr(arg_name, arg_args) => {
        let mut new_arg_args = Vec::new();
        for field in arg_args {
          let var_name = match field {
            Pattern::Ctr(..) | Pattern::Tup(..) | Pattern::Num(..) | Pattern::Var(None) => {
              make_var_name(&mut var_count)
            }
            Pattern::Var(Some(nam)) => nam.clone(),
            Pattern::List(..) => unreachable!(),
          };
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
      Pattern::Var(None) => {
        new_pats.push(Pattern::Var(None));
      }
      Pattern::Var(Some(nam)) => {
        new_pats.push(Pattern::Var(Some(nam.clone())));
        new_body_args.push(Term::Var { nam: nam.clone() });
      }
      Pattern::Num(_) => {
        // How to do this if num can be either a number or some sort of lambda? add a match? separate both cases?
        todo!();
      }
      Pattern::List(..) => unreachable!(),
    }
  }
  let new_body = Term::call(Term::Ref { def_id: new_split_def_id }, new_body_args);
  Rule { pats: new_pats, body: new_body, origin: rule.origin }
}

/// Makes one of the new rules, flattening one layer of the original pattern.
fn make_split_rule(old_rule: &Rule, other_rule: &Rule, def_names: &DefNames) -> Rule {
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
      // We checked before that these two have the same constructor and match together
      (Pattern::Ctr(_, _), Pattern::Ctr(_, other_arg_args)) => {
        for other_field in other_arg_args {
          new_pats.push(other_field.clone());
        }
      }
      (Pattern::Ctr(rule_arg_name, rule_arg_args), Pattern::Var(Some(other_arg))) => {
        let mut new_ctr_args = vec![];
        for _ in 0 .. rule_arg_args.len() {
          let new_nam = make_var_name(&mut var_count);
          new_ctr_args.push(Term::Var { nam: new_nam.clone() });
          new_pats.push(Pattern::Var(Some(new_nam)));
        }
        let def_ref = def_names.get_ref(rule_arg_name);
        let new_ctr = Term::call(def_ref, new_ctr_args);
        new_body.subst(other_arg, &new_ctr);
      }
      // Since numbers don't have subpatterns this should be unreachable.
      (Pattern::Num(..), _) => unreachable!(),
      (Pattern::Tup(_, _), Pattern::Tup(fst, snd)) => {
        new_pats.push(*fst.clone());
        new_pats.push(*snd.clone());
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
      (Pattern::Var(Some(_)), _) => new_pats.push(other_arg.clone()),
      (Pattern::Var(None), _) => (),
      // Unreachable cases, we only call this function if we know the two patterns match together
      (Pattern::Ctr(..) | Pattern::Tup(..), Pattern::Ctr(..) | Pattern::Num(..) | Pattern::Tup(..)) => {
        unreachable!()
      }
      (Pattern::Ctr(_, ctr_fields), Pattern::Var(None)) => {
        for _ in ctr_fields {
          new_pats.push(Pattern::Var(None));
        }
      }
      (Pattern::Tup(..), Pattern::Var(None)) => {
        new_pats.push(Pattern::Var(None));
        new_pats.push(Pattern::Var(None));
      }
      (Pattern::List(..), _) | (_, Pattern::List(..)) => unreachable!(),
    }
  }
  Rule { pats: new_pats, body: new_body, origin: Origin::Generated }
}

fn make_var_name(var_count: &mut usize) -> Name {
  let nam = Name(format!("x${var_count}"));
  *var_count += 1;
  nam
}
