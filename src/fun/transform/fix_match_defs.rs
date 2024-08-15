use crate::{
  diagnostics::Diagnostics,
  fun::{Adts, Constructors, Ctx, Pattern, Rule, Term},
};

impl Ctx<'_> {
  /// Makes every pattern matching definition have correct a left-hand side.
  ///
  /// Does not check exhaustiveness of rules and type mismatches. (Inter-ctr/type proprieties)
  pub fn fix_match_defs(&mut self) -> Result<(), Diagnostics> {
    self.info.start_pass();

    for def in self.book.defs.values_mut() {
      let mut errs = vec![];

      let def_arity = def.arity();
      for rule in &mut def.rules {
        rule.fix_match_defs(def_arity, &self.book.ctrs, &self.book.adts, &mut errs);
      }

      for err in errs {
        self.info.add_function_error(err, def.name.clone(), def.source.clone());
      }
    }

    self.info.fatal(())
  }
}

impl Rule {
  fn fix_match_defs(&mut self, def_arity: usize, ctrs: &Constructors, adts: &Adts, errs: &mut Vec<String>) {
    if self.arity() != def_arity {
      errs.push(format!(
        "Incorrect pattern matching rule arity. Expected {} args, found {}.",
        def_arity,
        self.arity()
      ));
    }

    for pat in &mut self.pats {
      pat.resolve_pat(ctrs);
      pat.check_good_ctr(ctrs, adts, errs);
    }

    self.body.fix_match_defs(ctrs, adts, errs);
  }
}

impl Term {
  fn fix_match_defs(&mut self, ctrs: &Constructors, adts: &Adts, errs: &mut Vec<String>) {
    match self {
      Term::Def { def, nxt } => {
        let def_arity = def.arity();
        for rule in &mut def.rules {
          rule.fix_match_defs(def_arity, ctrs, adts, errs);
        }
        nxt.fix_match_defs(ctrs, adts, errs);
      }
      _ => {
        for children in self.children_mut() {
          children.fix_match_defs(ctrs, adts, errs);
        }
      }
    }
  }
}

impl Pattern {
  /// If a var pattern actually refers to an ADT constructor, convert it into a constructor pattern.
  fn resolve_pat(&mut self, ctrs: &Constructors) {
    if let Pattern::Var(Some(nam)) = self {
      if ctrs.contains_key(nam) {
        *self = Pattern::Ctr(std::mem::take(nam), vec![]);
      }
    }
    for child in self.children_mut() {
      child.resolve_pat(ctrs);
    }
  }

  /// Check that ADT constructor pats are correct, meaning defined in a `data` and with correct arity.
  fn check_good_ctr(&self, ctrs: &Constructors, adts: &Adts, errs: &mut Vec<String>) {
    if let Pattern::Ctr(nam, args) = self {
      if let Some(adt) = ctrs.get(nam) {
        let expected_arity = adts[adt].ctrs[nam].len();
        let found_arity = args.len();
        if expected_arity != found_arity {
          errs.push(format!(
            "Incorrect arity for constructor '{}' of type '{}' in pattern matching rule. Expected {} fields, found {}",
            nam, adt, expected_arity, found_arity
          ));
        }
      } else {
        errs.push(format!("Unbound constructor '{nam}' in pattern matching rule."));
      }
    }
    for child in self.children() {
      child.check_good_ctr(ctrs, adts, errs);
    }
  }
}
