use crate::{
  fun::{builtins, Name, Num, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};

impl Term {
  /// Converts lambda-encoded strings ending with String/nil to string literals.
  pub fn resugar_strings(&mut self, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => self.try_resugar_strings_with(Self::resugar_strings_scott),
      AdtEncoding::NumScott => self.try_resugar_strings_with(Self::resugar_strings_num_scott),
    }
  }

  /// Converts encoded strings to string literals using the provided extraction function.
  fn try_resugar_strings_with(&mut self, extract_fn: fn(&Term) -> Option<(char, &Term)>) {
    maybe_grow(|| {
      // Try to resugar nil or cons patterns. If unsuccessful, recurse into child terms.
      if !self.try_resugar_strings_nil() && !self.try_resugar_strings_cons(extract_fn) {
        for child in self.children_mut() {
          child.try_resugar_strings_with(extract_fn);
        }
      }
    })
  }

  /// Attempts to resugar a nil term (String/nil) to an empty string literal.
  fn try_resugar_strings_nil(&mut self) -> bool {
    matches!(self, Term::Ref { nam } if nam == builtins::SNIL).then(|| *self = Term::str("")).is_some()
  }

  /// Attempts to resugar a cons term to a string literal.
  fn try_resugar_strings_cons(&mut self, extract_fn: fn(&Term) -> Option<(char, &Term)>) -> bool {
    self
      .try_resugar_strings_cons_with(extract_fn)
      .or_else(|| self.try_resugar_strings_cons_common())
      .map(|str| *self = Term::str(&str))
      .is_some()
  }

  /// Attempts to resugar a cons term using the provided extraction function.
  fn try_resugar_strings_cons_with(&self, extract_fn: fn(&Term) -> Option<(char, &Term)>) -> Option<String> {
    extract_fn(self)
      .and_then(|(head_char, tail)| Self::build_strings_common(tail, head_char.to_string(), extract_fn))
  }

  /// Attempts to resugar a cons term using the common extraction method.
  fn try_resugar_strings_cons_common(&self) -> Option<String> {
    if let Term::App { tag: Tag::Static, fun, arg: tail } = self {
      if let Term::App { tag: Tag::Static, fun: inner_fun, arg: head } = fun.as_ref() {
        if let (Term::Ref { nam }, Term::Num { val: Num::U24(head_val) }) =
          (inner_fun.as_ref(), head.as_ref())
        {
          if nam == builtins::SCONS {
            let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
            return Self::build_strings_common(tail, head_char.to_string(), Self::extract_strings_common);
          }
        }
      }
    }
    None
  }

  /// Builds a string from a term structure using the provided extraction function.
  fn build_strings_common(
    term: &Term,
    mut s: String,
    extract_fn: fn(&Term) -> Option<(char, &Term)>,
  ) -> Option<String> {
    maybe_grow(|| {
      let mut current = term;
      loop {
        match current {
          // If we reach a nil term, we've completed the string
          Term::Ref { nam } if nam == builtins::SNIL => return Some(s),
          _ => {
            // Extract the next character and continue building the string
            let (head, next) = extract_fn(current).or_else(|| Self::extract_strings_common(current))?;
            s.push(head);
            current = next;
          }
        }
      }
    })
  }

  /// Extracts a character and the remaining term from a Scott-encoded string term.
  /// The structure of this function mimics the shape of the AST for easier visualization.
  fn resugar_strings_scott(term: &Term) -> Option<(char, &Term)> {
    if let Term::Lam { tag: Tag::Static, pat: outer_pat, bod } = term {
      if let Pattern::Var(None) = outer_pat.as_ref() {
        if let Term::Lam { tag: Tag::Static, pat: inner_pat, bod: inner_bod } = bod.as_ref() {
          if let Pattern::Var(Some(var_lam)) = inner_pat.as_ref() {
            if let Term::App { tag: Tag::Static, fun, arg: tail } = inner_bod.as_ref() {
              if let Term::App { tag: Tag::Static, fun: inner_fun, arg: head } = fun.as_ref() {
                if let (Term::Var { nam: var_app }, Term::Num { val: Num::U24(head_val) }) =
                  (inner_fun.as_ref(), head.as_ref())
                {
                  if var_lam == var_app {
                    let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
                    return Some((head_char, tail));
                  }
                }
              }
            }
          }
        }
      }
    }
    None
  }

  /// Extracts a character and the remaining term from a NumScott-encoded string term.
  /// The structure of this function mimics the shape of the AST for easier visualization.
  fn resugar_strings_num_scott(term: &Term) -> Option<(char, &Term)> {
    if let Term::Lam { tag: Tag::Static, pat, bod } = term {
      if let Pattern::Var(Some(var_lam)) = pat.as_ref() {
        if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() {
          if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
            if let Term::App { tag: Tag::Static, fun, arg } = fun.as_ref() {
              if let (
                Term::Var { nam: var_app },
                Term::Ref { nam: Name(ref_nam) },
                Term::Num { val: Num::U24(head_val) },
              ) = (fun.as_ref(), arg.as_ref(), head.as_ref())
              {
                if var_lam == var_app && ref_nam == builtins::SCONS_TAG_REF {
                  let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
                  return Some((head_char, tail));
                }
              }
            }
          }
        }
      }
    }
    None
  }

  /// Extracts a character and the remaining term from a common-encoded string term.
  /// The structure of this function mimics the shape of the AST for easier visualization.
  fn extract_strings_common(term: &Term) -> Option<(char, &Term)> {
    if let Term::App { tag: Tag::Static, fun, arg: tail } = term {
      if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
        if let (Term::Ref { nam }, Term::Num { val: Num::U24(head_val) }) = (fun.as_ref(), head.as_ref()) {
          if nam == builtins::SCONS {
            let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
            return Some((head_char, tail));
          }
        }
      }
    }
    None
  }
}
