use crate::ast::{
  hvm_lang::{DefNames, Pattern, SpannedPattern, SpannedRule, SpannedDefinition},
  DefId, Definition, DefinitionBook, Name, Rule, Term,
};
use hvmc::Val;
use std::collections::HashSet;

impl DefinitionBook {
  /// Splits definitions with nested pattern matching into multiple ones so that the maximum pattern matching depth is 1.
  pub fn flatten_rules(&mut self) {
    for i in 0 .. self.defs.len() {
      let (old_def, mut new_defs) = flatten_def(&self.defs[i], &mut self.def_names);
      // Replace the definition with its flattened version and add the extracted rules to the book.
      // We add the names for those definitions when they're first defined, so no need to add the names to the book here.
      self.defs[i] = old_def.into();
      self.defs.append(&mut new_defs);
    }
  }
}

/// Flattens a single definition.
/// Returns the original definition but flattened,
/// as well as the new ones that were split from the nested one, all of them also flattened.
fn flatten_def(def: &Definition, def_names: &mut DefNames) -> (Definition, Vec<SpannedDefinition>) {
  let mut new_defs: Vec<SpannedDefinition> = Vec::new();

  // For each extracted nested pattern, we create a new definition with a name based on this counter.
  let mut name_count = 0;
  // The indices of rules that have already been processed by the flattening step of a previous rule in this def.
  let mut skip: HashSet<usize> = HashSet::default();
  // The flattened rules we're building for this definition.
  // Old refers to the already existing definition, while new is the ones we're extracting from nested patterns.
  let mut old_def_rules: Vec<SpannedRule> = Vec::new();

  for i in 0 .. def.rules.len() {
    if !skip.contains(&i) {
      let rule = &def.rules[i];
      if must_split(rule) {
        let (old_rule, new_def, split_defs) =
          split_rule(&def.rules, i, &mut name_count, &mut skip, def_names);
        old_def_rules.push(old_rule.into());
        new_defs.push(new_def.into());
        new_defs.extend(split_defs);
      } else {
        old_def_rules.push((*def.rules[i].clone()).clone().into());
      }
    }
  }
  let old_def = Definition { def_id: def.def_id, rules: old_def_rules };
  (old_def, new_defs)
}

/// Returns true if a rule needs to be split to be made flat.
/// If a rule has nested patterns (matchable pattern inside another matchable pattern) then it must be split.
/// Ex: (Rule (CtrA (CtrB))) has a constructor inside a constructor
/// It must be separated into a new rule (Rule$0 (CtrB)) while changing the old pattern to (Rule (CtrA .x0)).
fn must_split(rule: &Rule) -> bool {
  for pat in &rule.pats {
    if let Pattern::Ctr(_, args) = &**pat {
      for arg in args {
        if matches!(&**arg, Pattern::Ctr(..) | Pattern::Num(..)) {
          return true;
        }
      }
    }
  }
  false
}

/// Returns whether when 'a' matches an expression 'b' will also always match together.
fn matches_together(a: &Rule, b: &Rule) -> bool {
  let mut a_implies_b = true;

  for (a_pat, b_pat) in a.pats.iter().zip(&b.pats) {
    match (&**a_pat, &**b_pat) {
      (Pattern::Var(..), _) => (),
      // The hvm also has this clause, but it's used to generate extra incorrect rules.
      // On the hvm this doesn't matter, but here it would mess with the ADTs.
      //(_, Pattern::Var(..)) => b_implies_a = false,
      (Pattern::Ctr(an, ..), Pattern::Ctr(bn, ..)) if an == bn => (),
      (Pattern::Ctr(..), Pattern::Ctr(..)) => {
        a_implies_b = false;
        break;
      }
      (Pattern::Num(a), Pattern::Num(b)) if a == b => (),
      (Pattern::Num(..), Pattern::Num(..)) => {
        a_implies_b = false;
        break;
      }
      // Other cases are when the pattern types don't match (like constructor with number)
      _ => {
        a_implies_b = false;
        break;
      }
    }
  }
  a_implies_b
}

/// Flattens a rule with nested pattern matching.
/// This creates at least one new definition from the top layer of flattening.
/// The new extracted definition might still have nested patterns,
/// so we flatten those until all patterns have only 1 layer of pattern matching.
/// Those recursive flattening steps return a vector of new subdefinitions which are also returned here.
/// If we encounter
fn split_rule(
  rules: &[SpannedRule],
  rule_idx: usize,
  name_count: &mut Val,
  skip: &mut HashSet<usize>,
  def_names: &mut DefNames,
) -> (Rule, Definition, Vec<SpannedDefinition>) {
  let mut var_count: Val = 0;
  let new_def_id = make_split_def_name(rules[rule_idx].def_id, name_count, def_names);
  let new_def = make_split_def(rules, rule_idx, new_def_id, &mut var_count, skip);
  let old_rule = make_rule_calling_split(&rules[rule_idx], new_def_id);
  // Recursively flatten the newly created definition
  let (new_def, split_defs) = flatten_def(&new_def, def_names);
  (old_rule, new_def, split_defs)
}

/// Generates a unique name for the definition we're extracting from a nested pattern matching of the old definition.
/// Inserts the name in the DefBook's name registry and return its Id.
fn make_split_def_name(old_def_id: DefId, name_count: &mut Val, def_names: &mut DefNames) -> DefId {
  let num = *name_count;
  *name_count += 1;
  let old_def_name = def_names.name(&old_def_id).unwrap();
  let new_def_name = Name(format!("{}${}$", old_def_name, num));
  def_names.insert(new_def_name)
}

/// Make the definition coming from splitting a rule with nested patterns.
fn make_split_def(
  rules: &[SpannedRule],
  rule_idx: usize,
  new_def_id: DefId,
  var_count: &mut Val,
  skip: &mut HashSet<usize>,
) -> Definition {
  let mut new_def_rules: Vec<SpannedRule> = Vec::new();

  let rule = &rules[rule_idx];
  // For each rule that also matches with this one, we create a rule in the newly extracted definition.
  // We don't look back though, since all previous rules were already taken care of.
  for (j, other) in rules.iter().enumerate().skip(rule_idx) {
    let matches_together = matches_together(rule, other);
    if matches_together {
      skip.insert(j);
      let mut new_rule_pats = Vec::new();
      let mut new_rule_body = other.body.clone();
      for (rule_pat, other_pat) in rule.pats.iter().zip(&other.pats) {
        match (&**rule_pat, &**other_pat) {
          (Pattern::Ctr(..), Pattern::Ctr(_, pat_args)) => {
            new_rule_pats.extend(pat_args.clone());
          }
          (Pattern::Ctr(ctr_name, ctr_args), Pattern::Var(opat_name)) => {
            let mut new_ctr_args = vec![];
            for _ in 0 .. ctr_args.len() {
              let new_arg = Pattern::Var(Some(Name(format!(".x{}", var_count))));
              *var_count += 1;
              new_ctr_args.push(Term::from(&new_arg));
              new_rule_pats.push(new_arg.into());
            }
            let new_ctr = Term::call(Term::Var { nam: ctr_name.clone() }, new_ctr_args);
            if let Some(opat_name) = opat_name {
              new_rule_body.subst(opat_name, &new_ctr);
            }
          }
          (Pattern::Var(..), other_pat) => {
            new_rule_pats.push(other_pat.clone().into());
          }
          (Pattern::Num(..), Pattern::Num(..)) => (),
          (Pattern::Num(num), Pattern::Var(name)) => {
            if let Some(name) = name {
              new_rule_body.subst(name, &Term::Num { val: *num });
            }
          }
          // Not possible since we know it matches
          _ => {
            panic!("Internal error. Please report.");
          }
        }
      }
      let new_rule = Rule { def_id: new_def_id, pats: new_rule_pats, body: new_rule_body };
      new_def_rules.push(new_rule.into());
    }
  }
  debug_assert!(!new_def_rules.is_empty());

  Definition { def_id: new_def_id, rules: new_def_rules }
}

/// Make a rule that calls the new split definition to be used in place of the rules with nested patterns.
fn make_rule_calling_split(old_rule: &Rule, new_def_id: DefId) -> Rule {
  let mut var_count = 0;
  let mut old_rule_pats: Vec<SpannedPattern> = Vec::new();
  let mut old_rule_body_args: Vec<Term> = Vec::new();

  for pat in &old_rule.pats {
    match &**pat {
      Pattern::Var(name) => {
        old_rule_pats.push(pat.clone());
        // TODO: Test this
        old_rule_body_args.push(Term::Var { nam: name.as_ref().cloned().unwrap_or(Name::new("_")) }.into());
      }
      Pattern::Ctr(name, args) => {
        let mut new_pat_args = Vec::new();

        for field in args {
          let arg = match &**field {
            Pattern::Ctr(..) => {
              let nam = Name(format!(".x{}", var_count));
              var_count += 1;
              Pattern::Var(Some(nam))
            }
            Pattern::Var(..) => field.clone().inner,
            Pattern::Num(..) => {
              let nam = Name(format!(".x{}", var_count));
              var_count += 1;
              Pattern::Var(Some(nam))
            }
          };
          old_rule_body_args.push(Term::from(&arg).into());
          new_pat_args.push(arg.into());
        }

        old_rule_pats.push(Pattern::Ctr(name.clone(), new_pat_args).into());
      }
      Pattern::Num(..) => {
        old_rule_pats.push(pat.clone());
      }
    }
  }
  let old_rule_body = Term::call(Term::Ref { def_id: new_def_id }.into(), old_rule_body_args);

  Rule { def_id: old_rule.def_id, pats: old_rule_pats, body: old_rule_body.into() }
}
