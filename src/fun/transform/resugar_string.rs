use builtins::SCONS_TAG;

use crate::{
  fun::{builtins, Num, Pattern, Tag, Term},
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
      match self {
        // Nil: String/nil
        Term::Ref { nam } if nam == builtins::SNIL => *self = Term::str(""),
        // Cons: @x (x CONS_TAG <num> <str>)
        Term::Lam {
          tag: Tag::Static,
          pat: box Pattern::Var(Some(var_lam)),
          bod:
            box Term::App {
              tag: Tag::Static,
              fun:
                box Term::App {
                  tag: Tag::Static,
                  fun:
                    box Term::App {
                      tag: Tag::Static,
                      fun: box Term::Var { nam: var_app },
                      arg: box Term::Num { val: Num::U24(SCONS_TAG) },
                    },
                  arg: box Term::Num { val: Num::U24(head) },
                },
              arg: tail,
            },
        } if var_lam == var_app => {
          let head = char::from_u32(*head).unwrap_or('.');
          if let Some(str) = build_string_num_scott(tail, head.to_string()) {
            *self = Term::str(&str);
          } else {
            // Not a string term, keep as-is.
          }
        }
        // Cons: (String/cons <num> <str>)
        Term::App {
          tag: Tag::Static,
          fun:
            box Term::App {
              tag: Tag::Static,
              fun: box Term::Ref { nam },
              arg: box Term::Num { val: Num::U24(head) },
            },
          arg: tail,
        } if nam == builtins::SCONS => {
          let head = char::from_u32(*head).unwrap_or('.');
          if let Some(str) = build_string_num_scott(tail, head.to_string()) {
            *self = Term::str(&str);
          } else {
            // Not a string term, keep as-is.
          }
        }
        _ => (),
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
      match self {
        // Nil: String/nil
        Term::Ref { nam } if nam == builtins::SNIL => *self = Term::str(""),
        // Cons: @* @c (c <num> <str>)
        Term::Lam {
          tag: Tag::Static,
          pat: box Pattern::Var(None),
          bod:
            box Term::Lam {
              tag: Tag::Static,
              pat: box Pattern::Var(Some(nam_cons_lam)),
              bod:
                box Term::App {
                  tag: Tag::Static,
                  fun:
                    box Term::App {
                      tag: Tag::Static,
                      fun: box Term::Var { nam: nam_cons_app },
                      arg: box Term::Num { val: Num::U24(c) },
                    },
                  arg: nxt,
                },
            },
        } if nam_cons_lam == nam_cons_app => {
          let head = char::from_u32(*c).unwrap_or('.');
          if let Some(str) = build_string_scott(nxt, head.to_string()) {
            *self = Term::str(&str);
          } else {
            // Not a string term, keep as-is.
          }
        }
        // Cons: (String/cons <num> <str>)
        Term::App {
          tag: Tag::Static,
          fun:
            box Term::App {
              tag: Tag::Static,
              fun: box Term::Ref { nam },
              arg: box Term::Num { val: Num::U24(c) },
            },
          arg: nxt,
        } if nam == builtins::SCONS => {
          let head = char::from_u32(*c).unwrap_or('.');
          if let Some(str) = build_string_scott(nxt, head.to_string()) {
            *self = Term::str(&str);
          } else {
            // Not a string term, keep as-is.
          }
        }
        _ => (),
      }

      for child in self.children_mut() {
        child.resugar_strings_scott();
      }
    })
  }
}

fn build_string_num_scott(term: &Term, mut s: String) -> Option<String> {
  maybe_grow(|| {
    match term {
      // Nil: String/nil
      Term::Ref { nam } if nam == builtins::SNIL => Some(s),
      // Cons: @x (x CONS_TAG <num> <str>)
      Term::Lam {
        tag: Tag::Static,
        pat: box Pattern::Var(Some(var_lam)),
        bod:
          box Term::App {
            tag: Tag::Static,
            fun:
              box Term::App {
                tag: Tag::Static,
                fun:
                  box Term::App {
                    tag: Tag::Static,
                    fun: box Term::Var { nam: var_app },
                    arg: box Term::Num { val: Num::U24(SCONS_TAG) },
                  },
                arg: box Term::Num { val: Num::U24(head) },
              },
            arg: tail,
          },
      } if var_lam == var_app => {
        // New string character, append and recurse
        let head = char::from_u32(*head).unwrap_or('.');
        s.push(head);
        build_string_num_scott(tail, s)
      }
      // Cons: (String/cons <num> <str>)
      Term::App {
        tag: Tag::Static,
        fun:
          box Term::App {
            tag: Tag::Static,
            fun: box Term::Ref { nam },
            arg: box Term::Num { val: Num::U24(head) },
          },
        arg: tail,
      } if nam == builtins::SCONS => {
        // New string character, append and recurse
        let head = char::from_u32(*head).unwrap_or('.');
        s.push(head);
        build_string_num_scott(tail, s)
      }
      _ => {
        // Not a string term, stop
        None
      }
    }
  })
}

fn build_string_scott(term: &Term, mut s: String) -> Option<String> {
  maybe_grow(|| {
    match term {
      // Nil: String/nil
      Term::Ref { nam } if nam == builtins::SNIL => Some(s),
      // Cons: @* @c (c <num> <str>)
      Term::Lam {
        tag: Tag::Static,
        pat: box Pattern::Var(None),
        bod:
          box Term::Lam {
            tag: Tag::Static,
            pat: box Pattern::Var(Some(nam_cons_lam)),
            bod:
              box Term::App {
                tag: Tag::Static,
                fun:
                  box Term::App {
                    tag: Tag::Static,
                    fun: box Term::Var { nam: nam_cons_app },
                    arg: box Term::Num { val: Num::U24(c) },
                  },
                arg: nxt,
              },
          },
      } if nam_cons_lam == nam_cons_app => {
        // New string character, append and recurse
        let head = char::from_u32(*c).unwrap_or('.');
        s.push(head);
        build_string_scott(nxt, s)
      }
      // Cons: (String/cons <num> <str>)
      Term::App {
        tag: Tag::Static,
        fun:
          box Term::App {
            tag: Tag::Static,
            fun: box Term::Ref { nam },
            arg: box Term::Num { val: Num::U24(c) },
          },
        arg: nxt,
      } if nam == builtins::SCONS => {
        // New string character, append and recurse
        let head = char::from_u32(*c).unwrap_or('.');
        s.push(head);
        build_string_scott(nxt, s)
      }
      _ => {
        // Not a string term, stop
        None
      }
    }
  })
}
