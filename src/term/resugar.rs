use super::{
  net_to_term::{ReadbackError, Reader},
  transform::encode_adts::adt_field_tag,
  Adt, DefId, Name, Pattern, Tag, Term, LIST, LNIL, SNIL, STRING,
};
use std::borrow::BorrowMut;

impl<'a> Reader<'a> {
  pub fn resugar_adts(&mut self, term: &mut Term) {
    match term {
      Term::Lam { tag: Tag::Named(adt_name), bod, .. } | Term::Chn { tag: Tag::Named(adt_name), bod, .. } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          return self.resugar_adts(bod);
        };

        self.resugar_adt_cons(term, adt, adt_name);

        match adt_name.0.as_ref() {
          STRING => {
            let snil = &self.book.def_names.name_to_id[&Name::new(SNIL)];
            *term = Self::resugar_string(term, snil);
          }
          LIST => {
            let lnil = &self.book.def_names.name_to_id[&Name::new(LNIL)];
            *term = Self::resugar_list(term, lnil);
          }
          _ => {}
        }
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
      let adt_field_tag = adt_field_tag(adt_name, ctr, field);

      match cur {
        Term::App { tag: Tag::Named(tag_name), .. } if tag_name == &adt_field_tag => {
          let Term::App { tag, fun, .. } = cur.borrow_mut() else { unreachable!() };
          *tag = Tag::Static;
          cur = fun;
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
                  self.error(ReadbackError::UnexpectedTag(adt_field_tag.clone(), tag.clone()));
                }

                let nam = self.namegen.unique();
                args.push(Pattern::Var(Some(nam.clone())));
                let tag = Tag::Named(adt_field_tag);
                *arm = Term::tagged_app(tag, std::mem::take(arm), Term::Var { nam });
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

  fn resugar_string(term: &mut Term, snil: &DefId) -> Term {
    match term {
      // (SCons Num tail)
      Term::App { fun: box Term::App { fun: ctr, arg: box Term::Num { val }, .. }, arg: tail, .. } => {
        let tail = Self::resugar_string(tail, snil);
        let char: String = unsafe { char::from_u32_unchecked(*val as u32) }.into();
        match tail {
          Term::Str { val: tail } => Term::Str { val: char + &tail },
          Term::Ref { def_id } if def_id == *snil => Term::Str { val: char },
          _ => {
            // FIXME: warnings are not good with this resugar
            // Just make the constructor again
            let fun =
              Term::App { tag: Tag::Static, fun: ctr.clone(), arg: Box::new(Term::Num { val: *val }) };
            Term::App { tag: Tag::Static, fun: Box::new(fun), arg: Box::new(tail) }
          }
        }
      }
      // (SNil)
      // Term::Ref { def_id } if def_id == snil => Term::Str { val: String::new() },
      other => std::mem::take(other),
    }
  }

  fn resugar_list(term: &mut Term, lnil: &DefId) -> Term {
    match term {
      // (LCons el tail)
      Term::App { fun: box Term::App { fun: ctr, arg: el, .. }, arg: tail, .. } => {
        let tail = Self::resugar_list(tail, lnil);
        if let Term::List { els: tail } = tail {
          let mut els = vec![*el.clone()];
          els.extend(tail);
          Term::List { els }
        } else {
          // Make the constructor again
          let fun = Term::App { tag: Tag::Static, fun: ctr.clone(), arg: el.clone() };
          Term::App { tag: Tag::Static, fun: Box::new(fun), arg: Box::new(tail) }
        }
      }
      // (LNil)
      Term::Ref { def_id } if def_id == lnil => Term::List { els: vec![] },
      other => std::mem::take(other),
    }
  }

  fn deref(&mut self, term: &mut Term) {
    while let Term::Ref { def_id } = term {
      let def = &self.book.defs[def_id];
      *term = def.rule().body.clone();
      term.fix_names(&mut self.namegen.id_counter, self.book);
    }
  }
}
