use crate::term::{Ctx, Name, Term};
use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone)]
pub enum RepeatedBindWarn {
  Rule(Name),
  Match(Name),
}

impl Display for RepeatedBindWarn {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RepeatedBindWarn::Rule(bind) => write!(f, "Repeated bind inside rule pattern: '{bind}'."),
      RepeatedBindWarn::Match(bind) => write!(f, "Repeated bind inside match arm: '{bind}'."),
    }
  }
}

impl Ctx<'_> {
  /// Checks that there are no unbound variables in all definitions.
  pub fn check_repeated_binds(&mut self) {
    for (def_name, def) in &self.book.defs {
      for rule in &def.rules {
        let mut binds = HashSet::new();
        for pat in &rule.pats {
          for nam in pat.named_binds() {
            if !binds.insert(nam) {
              self.info.warning(def_name.clone(), RepeatedBindWarn::Rule(nam.clone()));
            }
          }
        }

        let mut repeated_in_matches = Vec::new();
        rule.body.check_repeated_binds(&mut repeated_in_matches);

        for repeated in repeated_in_matches {
          self.info.warning(def_name.clone(), repeated);
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
            for nam in pat.named_binds() {
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
