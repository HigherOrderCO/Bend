use crate::{
  fun::{builtins, Name, Num, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};

impl Term {
  /// Converts lambda-encoded strings ending with String/nil to a string literals.
  pub fn resugar_strings(&mut self, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => self.resugar_strings_with(Self::try_resugar_cons_scott, Self::build_string_scott),
      AdtEncoding::NumScott => {
        self.resugar_strings_with(Self::try_resugar_cons_num_scott, Self::build_string_num_scott)
      }
    }
  }

  /// Helper method to resugar strings with specific cons and build functions.
  fn resugar_strings_with(
    &mut self,
    try_resugar_cons: fn(&mut Term) -> bool,
    build_string: fn(&Term, String) -> Option<String>,
  ) {
    maybe_grow(|| {
      if !self.try_resugar_nil() && !try_resugar_cons(self) && !self.try_resugar_cons_common(build_string) {
        for child in self.children_mut() {
          child.resugar_strings_with(try_resugar_cons, build_string);
        }
      }
    })
  }

  /// Attempts to resugar a nil term. Nil: String/nil
  fn try_resugar_nil(&mut self) -> bool {
    matches!(self, Term::Ref { nam } if nam == builtins::SNIL).then(|| *self = Term::str("")).is_some()
  }

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
  }

  /// Attempts to resugar a NumScott-encoded cons term.
  fn try_resugar_cons_num_scott(term: &mut Term) -> bool {
    // Pattern matching to extract the necessary components  Cons: (String/cons <num> <str>)
    let Term::Lam { tag: Tag::Static, pat, bod } = term else { return false };
    let Pattern::Var(Some(var_lam)) = pat.as_ref() else { return false };
    let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() else { return false };
    let Term::App { tag: Tag::Static, fun: inner_fun, arg: head } = fun.as_mut() else { return false };
    let Term::App { tag: Tag::Static, fun: inner_inner_fun, arg } = inner_fun.as_mut() else { return false };
    let (Term::Var { nam: var_app }, Term::Ref { nam }, Term::Num { val: Num::U24(head_val) }) =
      (inner_inner_fun.as_mut(), arg.as_mut(), head.as_mut())
    else {
      return false;
    };

    if var_lam != var_app || nam != builtins::SCONS_TAG_REF {
      return false;
    };

    let head_char = char::from_u32(*head_val).unwrap_or(char::REPLACEMENT_CHARACTER);
    Self::build_string_num_scott(tail, head_char.to_string()).map(|str| *term = Term::str(&str)).is_some()
  }

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
      }
    })
  }

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
      }
    })
  }
}

