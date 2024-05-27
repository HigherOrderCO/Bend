use crate::{
  fun::{builtins, Name, Num, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};

impl Term {
  /// Converts lambda-encoded strings ending with String/nil to a string literals.
  pub fn resugar_strings(&mut self, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => self.resugar_strings_scott(),
      AdtEncoding::NumScott => self.resugar_strings_num_scott(),
    }
  }

  /// Converts num-scott-encoded strings ending with String/nil to a string literals.
  fn resugar_strings_num_scott(&mut self) {
    maybe_grow(|| {
      // Search for a String/cons pattern in the term and try to build a string from that point on.
      // If successful, replace the term with the string.
      // If not, keep as-is.

      // Nil: String/nil
      if let Term::Ref { nam } = self {
        if nam == builtins::SNIL {
          *self = Term::str("");
        }
      }
      // Cons: @x (x CONS_TAG <num> <str>)
      if let Term::Lam { tag: Tag::Static, pat, bod } = self {
        if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
          if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
            if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
              if let Term::App { tag: Tag::Static, fun, arg } = fun.as_mut() {
                if let Term::Var { nam: var_app } = fun.as_mut() {
                  // if let Term::Num { val: Num::U24(SCONS_TAG) } = arg.as_mut() {
                  if let Term::Ref { nam: Name(nam) } = arg.as_mut() {
                    if let Term::Num { val: Num::U24(head) } = head.as_mut() {
                      if var_lam == var_app && nam == builtins::SCONS_TAG_REF {
                        let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                        if let Some(str) = build_string_num_scott(tail, head.to_string()) {
                          *self = Term::str(&str);
                        } else {
                          // Not a string term, keep as-is.
                        }
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
      if let Term::App { tag: Tag::Static, fun, arg: tail } = self {
        if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
          if let Term::Ref { nam } = fun.as_mut() {
            if let Term::Num { val: Num::U24(head) } = head.as_mut() {
              if nam == builtins::SCONS {
                let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                if let Some(str) = build_string_num_scott(tail, head.to_string()) {
                  *self = Term::str(&str);
                } else {
                  // Not a string term, keep as-is.
                }
              }
            }
          }
        }
      }

      for child in self.children_mut() {
        child.resugar_strings_num_scott();
      }
    })
  }

  /// Converts scott-encoded strings ending with String/nil to a string literals.
  fn resugar_strings_scott(&mut self) {
    maybe_grow(|| {
      // Search for a String/cons pattern in the term and try to build a string from that point on.
      // If successful, replace the term with the string.
      // If not, keep as-is.

      // Nil: String/nil
      if let Term::Ref { nam } = self {
        if nam == builtins::SNIL {
          *self = Term::str("");
        }
      }
      // Cons: @* @c (c <num> <str>)
      if let Term::Lam { tag: Tag::Static, pat, bod } = self {
        if let Pattern::Var(None) = pat.as_mut() {
          if let Term::Lam { tag: Tag::Static, pat, bod } = bod.as_mut() {
            if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
              if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
                if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
                  if let Term::Var { nam: var_app } = fun.as_mut() {
                    if let Term::Num { val: Num::U24(head) } = head.as_mut() {
                      if var_lam == var_app {
                        let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                        if let Some(str) = build_string_scott(tail, head.to_string()) {
                          *self = Term::str(&str);
                        } else {
                          // Not a string term, keep as-is.
                        }
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
      if let Term::App { tag: Tag::Static, fun, arg: tail } = self {
        if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
          if let Term::Ref { nam } = fun.as_mut() {
            if let Term::Num { val: Num::U24(head) } = head.as_mut() {
              if nam == builtins::SCONS {
                let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                if let Some(str) = build_string_num_scott(tail, head.to_string()) {
                  *self = Term::str(&str);
                } else {
                  // Not a string term, keep as-is.
                }
              }
            }
          }
        }
      }

      for child in self.children_mut() {
        child.resugar_strings_scott();
      }
    })
  }
}

fn build_string_num_scott(term: &Term, mut s: String) -> Option<String> {
  maybe_grow(|| {
    // Nil: String/nil
    if let Term::Ref { nam } = term {
      if nam == builtins::SNIL {
        return Some(s);
      }
    }
    // Cons: @x (x CONS_TAG <num> <str>)
    if let Term::Lam { tag: Tag::Static, pat, bod } = term {
      if let Pattern::Var(Some(var_lam)) = pat.as_ref() {
        if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() {
          if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
            if let Term::App { tag: Tag::Static, fun, arg } = fun.as_ref() {
              if let Term::Var { nam: var_app } = fun.as_ref() {
                if let Term::Ref { nam } = arg.as_ref() {
                  // if let Term::Num { val: Num::U24(SCONS_TAG) } = arg.as_ref() {
                  if let Term::Num { val: Num::U24(head) } = head.as_ref() {
                    if var_lam == var_app && nam == builtins::SCONS_TAG_REF {
                      // New string character, append and recurse
                      let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                      s.push(head);
                      return build_string_num_scott(tail, s);
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
    if let Term::App { tag: Tag::Static, fun, arg: tail } = term {
      if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
        if let Term::Ref { nam } = fun.as_ref() {
          if let Term::Num { val: Num::U24(head) } = head.as_ref() {
            if nam == builtins::SCONS {
              // New string character, append and recurse
              let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
              s.push(head);
              return build_string_num_scott(tail, s);
            }
          }
        }
      }
    }
    // Not a string term, stop
    None
  })
}

fn build_string_scott(term: &Term, mut s: String) -> Option<String> {
  maybe_grow(|| {
    // Nil: String/nil
    if let Term::Ref { nam } = term {
      if nam == builtins::SNIL {
        return Some(s);
      }
    }
    // Cons: @* @c (c <num> <str>)
    if let Term::Lam { tag: Tag::Static, pat, bod } = term {
      if let Pattern::Var(None) = pat.as_ref() {
        if let Term::Lam { tag: Tag::Static, pat, bod } = bod.as_ref() {
          if let Pattern::Var(Some(var_lam)) = pat.as_ref() {
            if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_ref() {
              if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
                if let Term::Var { nam: var_app } = fun.as_ref() {
                  if let Term::Num { val: Num::U24(head) } = head.as_ref() {
                    if var_lam == var_app {
                      // New string character, append and recurse
                      let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
                      s.push(head);
                      return build_string_scott(tail, s);
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
    if let Term::App { tag: Tag::Static, fun, arg: tail } = term {
      if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_ref() {
        if let Term::Ref { nam } = fun.as_ref() {
          if let Term::Num { val: Num::U24(head) } = head.as_ref() {
            if nam == builtins::SCONS {
              // New string character, append and recurse
              let head = char::from_u32(*head).unwrap_or(char::REPLACEMENT_CHARACTER);
              s.push(head);
              return build_string_scott(tail, s);
            }
          }
        }
      }
    }
    // Not a string term, stop
    None
  })
}
