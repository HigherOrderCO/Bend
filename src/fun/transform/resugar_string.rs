use crate::{
  fun::{builtins, Name, Num, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};

impl Term {
  /// Converts lambda-encoded strings ending with String/nil to string literals.
  /// This method chooses the appropriate resugaring function based on the ADT encoding.
  pub fn resugar_strings(&mut self, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => self.try_resugar_strings_with(Self::resugar_strings_scott),
      AdtEncoding::NumScott => self.try_resugar_strings_with(Self::resugar_strings_num_scott),
    }
  }

  /// Converts encoded strings to string literals using the provided extraction function.
  /// This method recursively processes the term structure.
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

<<<<<<< HEAD
  /// Attempts to resugar a Scott-encoded cons term.
  fn try_resugar_cons_scott(term: &mut Term) -> bool {
    // Pattern matching to extract the necessary components  Cons: @x (x CONS_TAG <num> <str>)
    let Term::Lam { tag: Tag::Static, pat: outer_pat, bod } = term else { return false };
    let Pattern::Var(None) = outer_pat.as_ref() else { return false };
    let Term::Lam { tag: Tag::Static, pat: inner_pat, bod: inner_bod } = bod.as_mut() else { return false };
    let Pattern::Var(Some(var_lam)) = inner_pat.as_ref() else { return false };
    let Term::App { tag: Tag::Static, fun, arg: tail } = inner_bod.as_mut() else { return false };
    let Term::App { tag: Tag::Static, fun: inner_fun, arg: head } = fun.as_mut() else { return false };
    let (Term::Var { nam: var_app }, Term::Num { val: Num::U24(head_val) }) =
      (inner_fun.as_mut(), head.as_mut())
    else {
      return false;
    };

    if var_lam != var_app {
      return false;
    };

    let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
    Self::build_string_scott(tail, head_char.to_string()).map(|str| *term = Term::str(&str)).is_some()
=======
  /// Attempts to resugar a cons term to a string literal.
  /// This method tries both the provided extraction function and the common extraction method.
  fn try_resugar_strings_cons(&mut self, extract_fn: fn(&Term) -> Option<(char, &Term)>) -> bool {
    self
      .try_resugar_strings_cons_with(extract_fn)
      .or_else(|| self.try_resugar_strings_cons_common())
      .map(|str| *self = Term::str(&str))
      .is_some()
>>>>>>> 6b0dd50b (#714Refactor Transform String Resugar Term Scott && keep  function mimics the shape of the AST for easier visualization)
  }

  /// Attempts to resugar a cons term using the provided extraction function.
  fn try_resugar_strings_cons_with(&self, extract_fn: fn(&Term) -> Option<(char, &Term)>) -> Option<String> {
    extract_fn(self)
      .and_then(|(head_char, tail)| Self::build_strings_common(tail, head_char.to_string(), extract_fn))
  }

<<<<<<< HEAD
  /// Attempts to resugar a common cons term.
  fn try_resugar_cons_common(&mut self, build_string_fn: fn(&Term, String) -> Option<String>) -> bool {
    let Term::App { tag: Tag::Static, fun, arg: tail } = self else { return false };
    let Term::App { tag: Tag::Static, fun: inner_fun, arg: head } = fun.as_mut() else { return false };
    let (Term::Ref { nam }, Term::Num { val: Num::U24(head_val) }) = (inner_fun.as_ref(), head.as_ref())
    else {
      return false;
    };

    if nam != builtins::SCONS {
      return false;
    };

    let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
    build_string_fn(tail, head_char.to_string()).map(|str| *self = Term::str(&str)).is_some()
  }

  /// common string building function that uses custom and default cons handlers.
  /// pattern keep if tree mimics the shape of the AST visualize the structure #713
  fn build_string<F>(term: &Term, mut s: String, cons_handler: F) -> Option<String>
  where
    F: Fn(&Term) -> Option<(char, &Term)>,
  {
    maybe_grow(|| {
      let mut current = term;
      loop {
        // Nil: String/nil
        if let Term::Ref { nam } = current {
          if nam == builtins::SNIL {
            return Some(s);
          }
        }

        // Try custom cons handler
        if let Some((head, tail)) = cons_handler(current) {
          s.push(head);
          current = tail;
          continue;
        }

        // Cons: @x (x CONS_TAG <num> <str>)
        if let Term::Lam { tag: Tag::Static, pat, bod } = current {
          if let Pattern::Var(Some(var_lam)) = pat.as_ref() {
            if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() {
              if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
                if let Term::App { tag: Tag::Static, fun, arg } = fun.as_ref() {
                  if let Term::Var { nam: var_app } = fun.as_ref() {
                    if let Term::Ref { nam } = arg.as_ref() {
                      if let Term::Num { val: Num::U24(head_val) } = head.as_ref() {
                        if var_lam == var_app && nam == builtins::SCONS_TAG_REF {
                          let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
                          s.push(head_char);
                          current = tail;
                          continue;
                        }
                      }
                    }
                  }
                }
              }
            }
=======
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
>>>>>>> 6b0dd50b (#714Refactor Transform String Resugar Term Scott && keep  function mimics the shape of the AST for easier visualization)
          }
        }

        // Cons: (String/cons <num> <str>)
        if let Term::App { tag: Tag::Static, fun, arg: tail } = current {
          if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
            if let Term::Ref { nam } = fun.as_ref() {
              if let Term::Num { val: Num::U24(head_val) } = head.as_ref() {
                if nam == builtins::SCONS {
                  let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
                  s.push(head_char);
                  current = tail;
                  continue;
                }
              }
            }
          }
        }

        // Not a string term, stop
        return None;
      }
    })
  }

<<<<<<< HEAD
  /// Builds a string from a NumScott-encoded term.
  fn build_string_num_scott(term: &Term, s: String) -> Option<String> {
    Self::build_string(term, s, |term| {
      let Term::Lam { tag: Tag::Static, pat, bod } = term else { return None };
      let Pattern::Var(Some(var_lam)) = pat.as_ref() else { return None };
      let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() else { return None };
      let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() else { return None };
      let Term::App { tag: Tag::Static, fun, arg } = fun.as_ref() else { return None };
      let (Term::Var { nam: var_app }, Term::Ref { nam }, Term::Num { val: Num::U24(head_val) }) =
        (fun.as_ref(), arg.as_ref(), head.as_ref())
      else {
        return None;
      };

      if var_lam == var_app && nam == builtins::SCONS_TAG_REF {
        let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
        Some((head_char, tail))
      } else {
        None
=======
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
>>>>>>> 6b0dd50b (#714Refactor Transform String Resugar Term Scott && keep  function mimics the shape of the AST for easier visualization)
      }
    }
    None
  }

<<<<<<< HEAD
  /// Builds a string from a Scott-encoded term.
  fn build_string_scott(term: &Term, s: String) -> Option<String> {
    Self::build_string(term, s, |term| {
      let Term::Lam { tag: Tag::Static, pat, bod } = term else { return None };
      let Pattern::Var(None) = pat.as_ref() else { return None };
      let Term::Lam { tag: Tag::Static, pat, bod } = bod.as_ref() else { return None };
      let Pattern::Var(Some(var_lam)) = pat.as_ref() else { return None };
      let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() else { return None };
      let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() else { return None };
      let (Term::Var { nam: var_app }, Term::Num { val: Num::U24(head_val) }) = (fun.as_ref(), head.as_ref())
      else {
        return None;
      };

      if var_lam == var_app {
        let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
        Some((head_char, tail))
      } else {
        None
=======
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
>>>>>>> 6b0dd50b (#714Refactor Transform String Resugar Term Scott && keep  function mimics the shape of the AST for easier visualization)
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

