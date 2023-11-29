use std::collections::HashSet;

use crate::term::{Book, DefNames, Name, RulePat};

impl Book {
  pub fn check_unbound_pats(&self) -> Result<(), String> {
    for def in self.defs.values() {
      for rule in &def.rules {
        for pat in &rule.pats {
          let unbounds = unbound_pats(pat, &self.def_names);
          if let Some(unbound) = unbounds.iter().next() {
            return Err(format!(
              "Unbound constructor '{}' in definition '{}'",
              unbound,
              self.def_names.name(&def.def_id).unwrap()
            ));
          }
        }
      }
    }
    Ok(())
  }
}

/// Given a possibly nested rule pattern, return a set of all used but not declared constructors.
pub fn unbound_pats(pat: &RulePat, def_names: &DefNames) -> HashSet<Name> {
  let mut unbounds = HashSet::new();
  let mut check = vec![pat];
  while let Some(pat) = check.pop() {
    match pat {
      RulePat::Ctr(nam, args) => {
        if !def_names.contains_name(nam) {
          unbounds.insert(nam.clone());
        }
        check.extend(args.iter());
      }
      RulePat::Var(_) => (),
    }
  }
  unbounds
}
