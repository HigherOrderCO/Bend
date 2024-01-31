use super::{
  builtins::{SCONS, SNIL},
  net_to_term::{ReadbackError, Reader},
  transform::encode_adts::adt_field_tag,
  Adt, Name, Pattern, Tag, Term,
};
use std::borrow::BorrowMut;

impl<'a> Reader<'a> {
  pub fn resugar_adts(&mut self, term: &mut Term) {
    match term {
      Term::Lam { tag, bod, .. } if *tag == Tag::string() => *term = self.resugar_string(std::mem::take(bod)),
      Term::Lam { tag, bod, .. } if *tag == Tag::list() => *term = self.resugar_list(std::mem::take(bod)),

      Term::Lam { tag: Tag::Named(adt_name), bod, .. } | Term::Chn { tag: Tag::Named(adt_name), bod, .. } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          return self.resugar_adts(bod);
        };

        self.resugar_adt_cons(term, adt, adt_name);
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => self.resugar_adts(bod),

      Term::App { tag: Tag::Named(adt_name), fun, arg } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          self.resugar_adts(fun);
          self.resugar_adts(arg);
          return;
        };

        self.resugar_adt_match(term, adt_name, adt);
      }

      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        self.resugar_adts(fst);
        self.resugar_adts(snd);
      }

      Term::Match { scrutinee, arms } => {
        self.resugar_adts(scrutinee);
        for (_, arm) in arms {
          self.resugar_adts(arm);
        }
      }

      Term::List { .. } => unreachable!(),
      Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Var { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era => {}
    }
  }

  /// Reconstructs adt-tagged lambdas as their constructors
  ///
  /// # Example
  ///
  /// ```hvm
  /// data Option = (Some val) | None
  ///
  /// // This value:
  /// (Some (Some None))
  ///
  /// // Gives the following readback:
  /// #Option λa #Option λ* #Option.Some.val (a #Option λb #Option λ* #Option.Some.val (b #Option λ* #Option λc c))
  ///
  /// // Which gets resugared as:
  /// (Some (Some None))
  /// ```
  fn resugar_adt_cons(&mut self, term: &mut Term, adt: &Adt, adt_name: &Name) {
    let mut app = &mut *term;
    let mut current_arm = None;

    for ctr in &adt.ctrs {
      self.deref(app);
      match app {
        Term::Lam { tag: Tag::Named(tag), nam, bod } if tag == adt_name => {
          if let Some(nam) = nam {
            if current_arm.is_some() {
              return self.error(ReadbackError::InvalidAdt);
            }
            current_arm = Some((nam.clone(), ctr))
          }
          app = &mut *bod;
        }
        _ => return self.error(ReadbackError::InvalidAdt),
      }
    }

    let Some((arm_name, (ctr, ctr_args))) = current_arm else {
      return self.error(ReadbackError::InvalidAdt);
    };

    let mut cur = &mut *app;

    for field in ctr_args.iter().rev() {
      self.deref(cur);
      let adt_field_tag = adt_field_tag(adt_name, ctr, field);

      match cur {
        Term::App { tag: Tag::Named(tag_name), .. } if tag_name == &adt_field_tag => {
          let Term::App { tag, fun, .. } = cur.borrow_mut() else { unreachable!() };
          *tag = Tag::Static;
          cur = fun
        }
        Term::App { tag, .. } => return self.error(ReadbackError::UnexpectedTag(adt_field_tag, tag.clone())),
        _ => return self.error(ReadbackError::InvalidAdt),
      }
    }

    match cur {
      Term::Var { nam } if nam == &arm_name => {}
      _ => return self.error(ReadbackError::InvalidAdt),
    }

    *cur = self.book.def_names.get_ref(ctr);
    *term = std::mem::take(app);

    self.resugar_adts(term);
  }

  /// Reconstructs adt-tagged applications as a match term
  ///
  /// # Example
  ///
  /// ```hvm
  /// data Option = (Some val) | None
  ///
  /// // This match expression:
  /// Option.and = @a @b match a {
  ///   Some: match b {
  ///     Some: 1
  ///     None: 2
  ///   }
  ///   None: 3
  /// }  
  ///
  /// // Gives the following readback:
  /// λa λb (#Option (a #Option.Some.val λ* λc #Option (c #Option.Some.val λ* 1 2) λ* 3) b)
  ///
  /// // Which gets resugared as:
  /// λa λb (match a {
  ///   (Some *): λc match c {
  ///     (Some *): 1;
  ///     (None)  : 2
  ///   };
  ///   (None): λ* 3
  /// } b)
  /// ```
  fn resugar_adt_match(&mut self, term: &mut Term, adt_name: &Name, adt: &Adt) {
    let mut cur = &mut *term;
    let mut arms = Vec::new();

    for (ctr, ctr_args) in adt.ctrs.iter().rev() {
      self.deref(cur);
      match cur {
        Term::App { tag: Tag::Named(tag), fun, arg } if tag == adt_name => {
          let mut args = Vec::new();
          let mut arm = arg.as_mut();

          for field in ctr_args {
            self.deref(arm);

            let adt_field_tag = adt_field_tag(adt_name, ctr, field);

            match arm {
              Term::Lam { tag: Tag::Named(tag), .. } if tag == &adt_field_tag => {
                let Term::Lam { nam, bod, .. } = arm.borrow_mut() else { unreachable!() };

                args.push(nam.clone().map_or(Pattern::Var(None), |x| Pattern::Var(Some(x))));
                arm = bod.as_mut();
              }
              _ => {
                if let Term::Lam { tag, .. } = arm {
                  self.error(ReadbackError::UnexpectedTag(adt_field_tag.clone(), tag.to_owned()));
                }

                let nam = self.namegen.unique();
                args.push(Pattern::Var(Some(nam.clone())));
                let tag = Tag::Named(adt_field_tag);
                *arm = Term::tagged_app(tag, std::mem::take(&mut arm), Term::Var { nam });
              }
            }
          }

          arms.push((Pattern::Ctr(ctr.clone(), args), arm));
          cur = &mut *fun;
        }
        _ => return self.error(ReadbackError::InvalidAdtMatch),
      }
    }

    let scrutinee = Box::new(std::mem::take(cur));
    let arms = arms.into_iter().rev().map(|(pat, term)| (pat, std::mem::take(term))).collect();
    *term = Term::Match { scrutinee, arms };

    self.resugar_adts(term);
  }

  fn resugar_string(&mut self, term: Term) -> Term {
    fn go(term: Term, str_term: Term, rd: &mut Reader<'_>) -> Term {
      match term {
        Term::Lam { tag, bod, .. } if tag == Tag::string() => go(*bod, str_term, rd),
        Term::App { tag, arg, .. } if tag == Tag::string_scons_head() => match *arg {
          Term::Num { val } => match str_term {
            Term::Str { val: str } => {
              let char: String = unsafe { char::from_u32_unchecked(val as u32) }.into();
              Term::Str { val: char + &str }
            }
            term => Term::call(rd.book.def_names.get_ref(&Name::new(SCONS)), [Term::Num { val }, term]),
          },
          Term::Var { nam } => rd.recover_string_cons(str_term, Term::Var { nam }),
          mut arg => {
            rd.resugar_adts(&mut arg);
            rd.error(ReadbackError::InvalidStrTerm(arg.clone()));
            rd.recover_string_cons(str_term, arg)
          }
        },
        Term::App { fun, arg, .. } => go(*fun, go(*arg, str_term, rd), rd),
        Term::Var { .. } => str_term,
        other => {
          rd.error(ReadbackError::InvalidStrTerm(other));
          str_term
        }
      }
    }

    go(term, Term::Str { val: String::new() }, self)
  }

  fn resugar_list(&mut self, term: Term) -> Term {
    fn go(term: Term, list: &mut Vec<Term>, rd: &mut Reader<'_>) {
      match term {
        Term::Lam { tag, bod, .. } if tag == Tag::list() => go(*bod, list, rd),
        Term::App { tag, mut arg, .. } if tag == Tag::list_lcons_head() => match *arg {
          Term::Lam { .. } => {
            rd.resugar_adts(&mut arg);
            list.push(*arg);
          }
          Term::Var { .. } => list.push(*arg),
          arg => go(arg, list, rd),
        },
        Term::App { fun, arg, .. } => {
          go(*fun, list, rd);
          go(*arg, list, rd);
        }
        Term::Var { .. } => {}
        other => list.push(other),
      }
    }
    let mut els = Vec::new();
    go(term, &mut els, self);
    Term::List { els }
  }

  /// Recover string constructors when it is not possible to correctly readback a string
  fn recover_string_cons(&self, mut term: Term, cons: Term) -> Term {
    match term {
      Term::Str { val } if val.is_empty() => term = Term::Var { nam: Name::new(SNIL) },
      _ => {}
    };

    Term::call(self.book.def_names.get_ref(&Name::new(SCONS)), [cons, term])
  }

  fn deref(&mut self, term: &mut Term) {
    while let Term::Ref { def_id } = term {
      let def = &self.book.defs[def_id];
      def.assert_no_pattern_matching_rules();
      *term = def.rules[0].body.clone();
      term.fix_names(&mut self.namegen.id_counter, self.book);
    }
  }
}
