use super::{
  net_to_term::{ReadbackError, Reader},
  Adt, AdtEncoding, DefName, Pattern, Tag, Term, LCONS, LIST, LNIL, SNIL, STRING,
};
use std::borrow::BorrowMut;

impl<'a> Reader<'a> {
  pub fn resugar_adts(&mut self, term: &mut Term, adt_encoding: AdtEncoding) {
    match adt_encoding {
      // No way of resugaring simple scott encoded terms.
      AdtEncoding::Scott => (),
      AdtEncoding::TaggedScott => self.resugar_tagged_scott(term),
    }
  }

  pub fn resugar_tagged_scott(&mut self, term: &mut Term) {
    match term {
      Term::Lam { tag: Tag::Named(adt_name), bod, .. } | Term::Chn { tag: Tag::Named(adt_name), bod, .. } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          return self.resugar_tagged_scott(bod);
        };

        self.resugar_ctr_tagged_scott(term, adt, adt_name);

        match adt_name.0.as_str() {
          STRING => {
            *term = Self::resugar_string(term);
          }
          LIST => {
            *term = Self::resugar_list(term);
          }
          _ => {}
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => self.resugar_tagged_scott(bod),

      Term::App { tag: Tag::Named(adt_name), fun, arg } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          self.resugar_tagged_scott(fun);
          self.resugar_tagged_scott(arg);
          return;
        };

        self.resugar_match_tagged_scott(term, adt_name, adt);
      }

      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        self.resugar_tagged_scott(fst);
        self.resugar_tagged_scott(snd);
      }

      Term::Match { scrutinee, arms } => {
        self.resugar_tagged_scott(scrutinee);
        for (_, arm) in arms {
          self.resugar_tagged_scott(arm);
        }
      }

      Term::List { .. } => unreachable!(),
      Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Var { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Invalid => {}
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
  fn resugar_ctr_tagged_scott(&mut self, term: &mut Term, adt: &Adt, adt_name: &DefName) {
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
            current_arm = Some((nam.clone(), ctr));
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
      let expected_tag = Tag::adt_field(adt_name, ctr, field);

      match cur {
        Term::App { tag, .. } if tag == &expected_tag => {
          let Term::App { tag, fun, .. } = cur.borrow_mut() else { unreachable!() };
          *tag = Tag::Static;
          cur = fun;
        }
        Term::App { tag, .. } => return self.error(ReadbackError::UnexpectedTag(expected_tag, tag.clone())),
        _ => return self.error(ReadbackError::InvalidAdt),
      }
    }

    match cur {
      Term::Var { nam } if nam == &arm_name => {}
      _ => return self.error(ReadbackError::InvalidAdt),
    }

    *cur = Term::r#ref(ctr);
    *term = std::mem::take(app);

    self.resugar_tagged_scott(term);
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
  fn resugar_match_tagged_scott(&mut self, term: &mut Term, adt_name: &DefName, adt: &Adt) {
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

            let expected_tag = Tag::adt_field(adt_name, ctr, field);

            match arm {
              Term::Lam { tag, .. } if tag == &expected_tag => {
                let Term::Lam { nam, bod, .. } = arm.borrow_mut() else { unreachable!() };

                args.push(nam.clone().map_or(Pattern::Var(None), |x| Pattern::Var(Some(x))));
                arm = bod.as_mut();
              }
              _ => {
                if let Term::Lam { tag, .. } = arm {
                  self.error(ReadbackError::UnexpectedTag(expected_tag.clone(), tag.clone()));
                }

                let nam = self.namegen.unique();
                args.push(Pattern::Var(Some(nam.clone())));
                *arm = Term::tagged_app(expected_tag, std::mem::take(arm), Term::Var { nam });
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

    self.resugar_tagged_scott(term);
  }

  fn resugar_string(term: &mut Term) -> Term {
    match term {
      // (SCons Num tail)
      Term::App { fun: box Term::App { fun: ctr, arg: box Term::Num { val }, .. }, arg: tail, .. } => {
        let tail = Self::resugar_string(tail);
        let char: String = unsafe { char::from_u32_unchecked(*val as u32) }.into();
        match tail {
          Term::Str { val: tail } => Term::Str { val: char + &tail },
          // (SNil)
          Term::Ref { def_name } if def_name.as_str() == SNIL => Term::Str { val: char },
          _ => {
            // FIXME: warnings are not good with this resugar
            // Just make the constructor again
            let fun =
              Term::App { tag: Tag::Static, fun: ctr.clone(), arg: Box::new(Term::Num { val: *val }) };
            Term::App { tag: Tag::Static, fun: Box::new(fun), arg: Box::new(tail) }
          }
        }
      }
      other => std::mem::take(other),
    }
  }

  fn resugar_list(term: &mut Term) -> Term {
    match term {
      // (LCons el tail)
      Term::App {
        fun: box Term::App { fun: box Term::Ref { def_name: ctr }, arg: el, .. },
        arg: tail,
        ..
      } => {
        let tail = Self::resugar_list(tail);
        if ctr.as_str() == LCONS && let Term::List { els: tail } = tail {
          let mut els = vec![*el.clone()];
          els.extend(tail);
          Term::List { els }
        } else {
          // Make the constructor again
          Term::call(Term::Ref { def_name: ctr.clone() }, [*el.clone(), tail])
        }
      }
      // (LNil)
      Term::Ref { def_name } if def_name.as_str() == LNIL => Term::List { els: vec![] },
      other => std::mem::take(other),
    }
  }

  fn deref(&mut self, term: &mut Term) {
    while let Term::Ref { def_name } = term {
      let def = self.book.defs.get(def_name).unwrap();
      *term = def.rule().body.clone();
      term.fix_names(&mut self.namegen.id_counter, self.book);
    }
  }
}
