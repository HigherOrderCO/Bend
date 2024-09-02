use crate::{
  diagnostics::Diagnostics,
  fun::{Ctx, FanKind, Pattern, Tag, Term},
  maybe_grow,
};

impl Ctx<'_> {
  /// Checks that terms that cannot be typed are only used inside untyped functions.
  pub fn check_untyped_terms(&mut self) -> Result<(), Diagnostics> {
    for def in self.book.defs.values() {
      if def.check {
        for rule in def.rules.iter() {
          if let Err(e) = rule.body.check_untyped_terms() {
            self.info.add_function_error(e, def.name.clone(), def.source.clone());
          }
        }
      }
    }
    self.info.fatal(())
  }
}

impl Term {
  fn check_untyped_terms(&self) -> Result<(), String> {
    maybe_grow(|| {
      match self {
        Term::Lam { tag: Tag::Static, pat, .. } => pat.check_untyped_patterns()?,
        Term::Lam { tag: _, .. } => {
          return Err("Tagged lambda in type-checked function".to_string());
        }
        Term::Link { nam } => {
          return Err(format!("Unscoped variable '${nam}' in type-checked function"));
        }
        Term::App { tag: Tag::Static, .. } => {}
        Term::App { tag: _, .. } => {
          return Err("Tagged application in type-checked function".to_string());
        }
        Term::Fan { fan: FanKind::Dup, .. } => {
          return Err("Superposition term in type-checked function".to_string());
        }
        Term::Fan { fan: FanKind::Tup, tag: Tag::Static, .. } => {}
        Term::Fan { fan: FanKind::Tup, tag: _, .. } => {
          return Err("Tagged tuple in type-checked function".to_string());
        }
        Term::Let { pat, .. } => {
          pat.check_untyped_patterns()?;
        }
        _ => {}
      }
      for child in self.children() {
        child.check_untyped_terms()?;
      }
      Ok(())
    })
  }
}

impl Pattern {
  fn check_untyped_patterns(&self) -> Result<(), String> {
    maybe_grow(|| {
      match self {
        Pattern::Chn(x) => {
          return Err(format!("Unscoped variable bind '${x}' in type-checked function"));
        }
        Pattern::Fan(FanKind::Dup, Tag::Auto, _) => {}
        Pattern::Fan(FanKind::Dup, _, _) => {
          return Err("Tagged duplication in type-checked function".to_string());
        }
        Pattern::Fan(FanKind::Tup, Tag::Static, _) => {}
        Pattern::Fan(FanKind::Tup, _, _) => {
          return Err("Tagged tuple elimination in type-checked function".to_string());
        }
        _ => {}
      }
      for pat in self.children() {
        pat.check_untyped_patterns()?;
      }
      Ok(())
    })
  }
}
