use crate::{
  fun::{builtins, Num, Pattern, Tag, Term},
  maybe_grow,
};

impl Term {
  /// Converts lambda-encoded strings ending with String/nil to a string literals.
  pub fn resugar_strings(&mut self) {
    maybe_grow(|| {
      // Search for a String/cons pattern in the term and try to build a string from that point on.
      // If successful, replace the term with the string.
      // If not, keep as-is.
      match self {
        // Nil: String/nil
        Term::Ref { nam } if nam == builtins::SNIL => *self = Term::str(""),
        // Cons: @a @* (a <num> <str>)
        Term::Lam {
          tag: Tag::Static,
          pat: box Pattern::Var(Some(nam_cons_lam)),
          bod:
            box Term::Lam {
              tag: Tag::Static,
              pat: box Pattern::Var(None),
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
          if let Some(str) = build_string(nxt, head.to_string()) {
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
          if let Some(str) = build_string(nxt, head.to_string()) {
            *self = Term::str(&str);
          } else {
            // Not a string term, keep as-is.
          }
        }
        _ => (),
      }

      for child in self.children_mut() {
        child.resugar_strings();
      }
    })
  }
}

fn build_string(term: &Term, mut s: String) -> Option<String> {
  maybe_grow(|| {
    match term {
      // Nil: String/nil
      Term::Ref { nam } if nam == builtins::SNIL => Some(s),
      // Cons: @a @* (a <num> <str>)
      Term::Lam {
        tag: Tag::Static,
        pat: box Pattern::Var(Some(nam_cons_lam)),
        bod:
          box Term::Lam {
            tag: Tag::Static,
            pat: box Pattern::Var(None),
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
        build_string(nxt, s)
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
        build_string(nxt, s)
      }
      _ => {
        // Not a string term, stop
        None
      }
    }
  })
}
