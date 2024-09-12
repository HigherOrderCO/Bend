use crate::{
  fun::{builtins, Pattern, Tag, Term},
  maybe_grow, AdtEncoding,
};

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

      // Nil: List/nil
      if let Term::Ref { nam } = self {
        if nam == builtins::LNIL {
          *self = Term::List { els: vec![] };
        }
      }
      // Cons: @x (x CONS_TAG <term> <term>)
      if let Term::Lam { tag: Tag::Static, pat, bod } = self {
        if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
          if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
            if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
              if let Term::App { tag: Tag::Static, fun, arg } = fun.as_mut() {
                if let Term::Var { nam: var_app } = fun.as_mut() {
                  if let Term::Ref { nam } = arg.as_mut() {
                    if var_lam == var_app && nam == builtins::LCONS_TAG_REF {
                      let l = build_list_num_scott(tail.as_mut(), vec![std::mem::take(head)]);
                      match l {
                        Ok(l) => *self = Term::List { els: l.into_iter().map(|x| *x).collect() },
                        // Was not a list term, keep as-is.
                        Err(mut l) => {
                          *head = l.pop().unwrap();
                          assert!(l.is_empty())
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
      // Cons: (List/Cons <term> <term>)
      if let Term::App { tag: Tag::Static, fun, arg: tail } = self {
        if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
          if let Term::Ref { nam } = fun.as_mut() {
            if nam == builtins::LCONS {
              let l = build_list_num_scott(tail.as_mut(), vec![std::mem::take(head)]);
              match l {
                Ok(l) => *self = Term::List { els: l.into_iter().map(|x| *x).collect() },
                // Was not a list term, keep as-is.
                Err(mut l) => {
                  *head = l.pop().unwrap();
                  assert!(l.is_empty())
                }
              }
            }
          }
        }
      }

      for child in self.children_mut() {
        child.resugar_lists_num_scott();
      }
    })
  }

  /// Converts scott-encoded lists ending with List/Nil to list literals.
  fn resugar_lists_scott(&mut self) {
    maybe_grow(|| {
      // Searc      h for a List/Cons pattern in the term and try to build a list from that point on.
      // If successful, replace the term with the list.
      // If not, keep as-is.

      // Nil: List/nil
      if let Term::Ref { nam } = self {
        if nam == builtins::LNIL {
          *self = Term::List { els: vec![] };
        }
      }
      // Cons: @* @c (c <term> <term>)
      if let Term::Lam { tag: Tag::Static, pat, bod } = self {
        if let Pattern::Var(None) = pat.as_mut() {
          if let Term::Lam { tag: Tag::Static, pat, bod } = bod.as_mut() {
            if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
              if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
                if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
                  if let Term::Var { nam: var_app } = fun.as_mut() {
                    if var_lam == var_app {
                      let l = build_list_scott(tail.as_mut(), vec![std::mem::take(head)]);
                      match l {
                        Ok(l) => *self = Term::List { els: l.into_iter().map(|x| *x).collect() },
                        // Was not a list term, keep as-is.
                        Err(mut l) => {
                          *head = l.pop().unwrap();
                          assert!(l.is_empty())
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
      // Cons: (List/Cons <term> <term>)
      if let Term::App { tag: Tag::Static, fun, arg: tail } = self {
        if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
          if let Term::Ref { nam } = fun.as_mut() {
            if nam == builtins::LCONS {
              let l = build_list_scott(tail.as_mut(), vec![std::mem::take(head)]);
              match l {
                Ok(l) => *self = Term::List { els: l.into_iter().map(|x| *x).collect() },
                // Was not a list term, keep as-is.
                Err(mut l) => {
                  *head = l.pop().unwrap();
                  assert!(l.is_empty())
                }
              }
            }
          }
        }
      }

      for child in self.children_mut() {
        child.resugar_lists_scott();
      }
    })
  }
}

// TODO: We have to do weird manipulations with Box<Term> because of the borrow checker.
// When we used use box patterns this was a way simpler match statement.
#[allow(clippy::vec_box)]
fn build_list_num_scott(term: &mut Term, mut l: Vec<Box<Term>>) -> Result<Vec<Box<Term>>, Vec<Box<Term>>> {
  maybe_grow(|| {
    // Nil: List/nil
    if let Term::Ref { nam } = term {
      if nam == builtins::LNIL {
        return Ok(l);
      }
    }
    // Cons: @x (x CONS_TAG <term> <term>)
    if let Term::Lam { tag: Tag::Static, pat, bod } = term {
      if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
        if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
          if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
            if let Term::App { tag: Tag::Static, fun, arg } = fun.as_mut() {
              if let Term::Var { nam: var_app } = fun.as_mut() {
                if let Term::Ref { nam } = arg.as_mut() {
                  if var_lam == var_app && nam == builtins::LCONS_TAG_REF {
                    // New list element, append and recurse
                    l.push(std::mem::take(head));
                    let l = build_list_num_scott(tail, l);
                    match l {
                      Ok(l) => return Ok(l),
                      Err(mut l) => {
                        // If it wasn't a list, we have to put it back.
                        *head = l.pop().unwrap();
                        return Err(l);
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
    // Cons: (List/Cons <term> <term>)
    if let Term::App { tag: Tag::Static, fun, arg: tail } = term {
      if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
        if let Term::Ref { nam } = fun.as_mut() {
          if nam == builtins::LCONS {
            // New list element, append and recurse
            l.push(std::mem::take(head));
            let l = build_list_num_scott(tail, l);
            match l {
              Ok(l) => return Ok(l),
              Err(mut l) => {
                // If it wasn't a list, we have to put it back.
                *head = l.pop().unwrap();
                return Err(l);
              }
            }
          }
        }
      }
    }
    // Not a list term, stop
    Err(l)
  })
}

#[allow(clippy::vec_box)]
fn build_list_scott(term: &mut Term, mut l: Vec<Box<Term>>) -> Result<Vec<Box<Term>>, Vec<Box<Term>>> {
  maybe_grow(|| {
    // Nil: List/nil
    if let Term::Ref { nam } = term {
      if nam == builtins::LNIL {
        return Ok(l);
      }
    }
    // Cons: @* @c (c <term> <term>)
    if let Term::Lam { tag: Tag::Static, pat, bod } = term {
      if let Pattern::Var(None) = pat.as_mut() {
        if let Term::Lam { tag: Tag::Static, pat, bod } = bod.as_mut() {
          if let Pattern::Var(Some(var_lam)) = pat.as_mut() {
            if let Term::App { tag: Tag::Static, fun, arg: tail } = bod.as_mut() {
              if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
                if let Term::Var { nam: var_app } = fun.as_mut() {
                  if var_lam == var_app {
                    // New list element, append and recurse
                    l.push(std::mem::take(head));
                    let l = build_list_scott(tail, l);
                    match l {
                      Ok(l) => return Ok(l),
                      Err(mut l) => {
                        // If it wasn't a list, we have to put it back.
                        *head = l.pop().unwrap();
                        return Err(l);
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
    // Cons: (List/Cons <term> <term>)
    if let Term::App { tag: Tag::Static, fun, arg: tail } = term {
      if let Term::App { tag: Tag::Static, fun, arg: head } = fun.as_mut() {
        if let Term::Ref { nam } = fun.as_mut() {
          if nam == builtins::LCONS {
            // New list element, append and recurse
            l.push(std::mem::take(head));
            let l = build_list_scott(tail, l);
            match l {
              Ok(l) => return Ok(l),
              Err(mut l) => {
                // If it wasn't a list, we have to put it back.
                *head = l.pop().unwrap();
                return Err(l);
              }
            }
          }
        }
      }
    }
    // Not a list term, stop
    Err(l)
  })
}

