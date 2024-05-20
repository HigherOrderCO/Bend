use crate::{
  fun::{builtins, Num, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};
use builtins::LCONS_TAG;

impl Term {
  /// Converts lambda-encoded lists ending with List/Nil to list literals.
  pub fn resugar_lists(&mut self, adt_encoding: AdtEncoding) {
    match adt_encoding {
      AdtEncoding::Scott => self.resugar_lists_scott(),
      AdtEncoding::NumScott => self.resugar_lists_num_scott(),
    }
  }

  /// Converts num-scott-encoded lists ending with List/Nil to list literals.
  fn resugar_lists_num_scott(&mut self) {
    maybe_grow(|| {
      // Search for a List/Cons pattern in the term and try to build a list from that point on.
      // If successful, replace the term with the list.
      // If not, keep as-is.
      match self {
        // Nil: List/nil
        Term::Ref { nam } if nam == builtins::LNIL => *self = Term::List { els: vec![] },
        // Cons: @x (x CONS_TAG <term> <term>)
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
                      arg: box Term::Num { val: Num::U24(LCONS_TAG) },
                    },
                  arg: head,
                },
              arg: tail,
            },
        } if var_lam == var_app => {
          head.resugar_lists_num_scott();
          if let Some(els) = build_list_num_scott(tail, vec![head]) {
            *self = Term::List { els };
          } else {
            // Not a list term, keep as-is.
          }
        }
        // Cons: (List/Cons <term> <term>)
        Term::App {
          tag: Tag::Static,
          fun: box Term::App { tag: Tag::Static, fun: box Term::Ref { nam }, arg: head },
          arg: tail,
        } if nam == builtins::LCONS => {
          if let Some(els) = build_list_num_scott(tail, vec![head]) {
            *self = Term::List { els };
          } else {
            // Not a list term, keep as-is.
          }
        }
        _ => (),
      }

      for child in self.children_mut() {
        child.resugar_lists_num_scott();
      }
    })
  }

  /// Converts scott-encoded lists ending with List/Nil to list literals.
  fn resugar_lists_scott(&mut self) {
    maybe_grow(|| {
      // Search for a List/Cons pattern in the term and try to build a list from that point on.
      // If successful, replace the term with the list.
      // If not, keep as-is.
      match self {
        // Nil: List/nil
        Term::Ref { nam } if nam == builtins::LNIL => *self = Term::List { els: vec![] },
        // Cons: @* @c (c <term> <term>)
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
                  fun: box Term::App { tag: Tag::Static, fun: box Term::Var { nam: nam_cons_app }, arg: head },
                  arg: tail,
                },
            },
        } if nam_cons_lam == nam_cons_app => {
          head.resugar_lists_scott();
          if let Some(els) = build_list_scott(tail, vec![head]) {
            *self = Term::List { els };
          } else {
            // Not a list term, keep as-is.
          }
        }
        // Cons: (List/Cons <term> <term>)
        Term::App {
          tag: Tag::Static,
          fun: box Term::App { tag: Tag::Static, fun: box Term::Ref { nam }, arg: head },
          arg: tail,
        } if nam == builtins::LCONS => {
          if let Some(els) = build_list_scott(tail, vec![head]) {
            *self = Term::List { els };
          } else {
            // Not a list term, keep as-is.
          }
        }
        _ => (),
      }

      for child in self.children_mut() {
        child.resugar_lists_scott();
      }
    })
  }
}

fn build_list_num_scott(term: &mut Term, mut s: Vec<&mut Term>) -> Option<Vec<Term>> {
  maybe_grow(|| {
    match term {
      // Nil: List/Nil
      Term::Ref { nam } if nam == builtins::LNIL => Some(s.into_iter().map(std::mem::take).collect()),
      // Cons: @x (x CONS_TAG <term> <term>)
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
                    arg: box Term::Num { val: Num::U24(LCONS_TAG) },
                  },
                arg: head,
              },
            arg: tail,
          },
      } if var_lam == var_app => {
        // New list element, append and recurse
        s.push(head.as_mut());
        build_list_num_scott(tail, s)
      }
      // Cons: (List/cons <term> <term>)
      Term::App {
        tag: Tag::Static,
        fun: box Term::App { tag: Tag::Static, fun: box Term::Ref { nam }, arg: head },
        arg: tail,
      } if nam == builtins::LCONS => {
        // New list element, append and recurse
        s.push(head.as_mut());
        build_list_num_scott(tail, s)
      }
      _ => {
        // Not a list term, stop
        None
      }
    }
  })
}

fn build_list_scott(term: &mut Term, mut s: Vec<&mut Term>) -> Option<Vec<Term>> {
  maybe_grow(|| {
    match term {
      // Nil: List/Nil
      Term::Ref { nam } if nam == builtins::LNIL => Some(s.into_iter().map(std::mem::take).collect()),
      // Cons: @* @c (c <term> <term>)
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
                fun: box Term::App { tag: Tag::Static, fun: box Term::Var { nam: nam_cons_app }, arg: head },
                arg: tail,
              },
          },
      } if nam_cons_lam == nam_cons_app => {
        // New list element, append and recurse
        s.push(head.as_mut());
        build_list_scott(tail, s)
      }
      // Cons: (List/cons <term> <term>)
      Term::App {
        tag: Tag::Static,
        fun: box Term::App { tag: Tag::Static, fun: box Term::Ref { nam }, arg: head },
        arg: tail,
      } if nam == builtins::LCONS => {
        // New list element, append and recurse
        s.push(head.as_mut());
        build_list_scott(tail, s)
      }
      _ => {
        // Not a list term, stop
        None
      }
    }
  })
}
