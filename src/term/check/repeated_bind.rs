use crate::{
  diagnostics::{ToStringVerbose, WarningType},
  term::{Ctx, Name, Term},
};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum RepeatedBindWarn {
  Rule(Name),
  Match(Name),
}

impl Ctx<'_> {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_repeated_binds(&mut self) {
    for (def_name, def) in &self.book.defs {
      for rule in &def.rules {
        let mut binds = HashSet::new();
        for pat in &rule.pats {
          for nam in pat.binds().flatten() {
            if !binds.insert(nam) {
              self.info.add_rule_warning(
                RepeatedBindWarn::Rule(nam.clone()),
                WarningType::RepeatedBind,
                def_name.clone(),
              );
            }
          }
        }

        let mut repeated_in_matches = Vec::new();
        rule.body.check_repeated_binds(&mut repeated_in_matches);

        for warn in repeated_in_matches {
          self.info.add_rule_warning(warn, WarningType::RepeatedBind, def_name.clone());
        }
      }
    }
  }
}

impl Term {
  pub fn check_repeated_binds(&self, repeated: &mut Vec<RepeatedBindWarn>) {
    Term::recursive_call(|| match self {
      Term::Mat { args, rules } => {
        for arg in args {
          arg.check_repeated_binds(repeated);
        }

        for rule in rules {
          let mut binds = HashSet::new();
          for pat in &rule.pats {
            for nam in pat.binds().flatten() {
              if !binds.insert(nam) {
                repeated.push(RepeatedBindWarn::Match(nam.clone()));
              }
            }
          }
        }
      }
      _ => {
        for child in self.children() {
          child.check_repeated_binds(repeated);
        }
      }
    })
  }
}

impl ToStringVerbose for RepeatedBindWarn {
  fn to_string_verbose(&self, _verbose: bool) -> String {
    match self {
      RepeatedBindWarn::Rule(bind) => format!("Repeated bind inside rule pattern: '{bind}'."),
      RepeatedBindWarn::Match(bind) => format!("Repeated bind inside match arm: '{bind}'."),
    }
  }
}
