use crate::term::{net_to_term::ReadbackError, Adt, AdtEncoding, Book, Name, Pattern, Tag, Term};

impl Term {
  pub fn resugar_adts(&mut self, book: &Book, adt_encoding: AdtEncoding) -> Vec<ReadbackError> {
    let mut errs = Default::default();
    match adt_encoding {
      // No way of resugaring simple scott encoded terms.
      AdtEncoding::Scott => (),
      AdtEncoding::TaggedScott => self.resugar_tagged_scott(book, &mut errs),
    };
    errs
  }

  fn resugar_tagged_scott(&mut self, book: &Book, errs: &mut Vec<ReadbackError>) {
    match self {
      Term::Lam { tag: Tag::Named(adt_name), bod, .. } | Term::Chn { tag: Tag::Named(adt_name), bod, .. } => {
        if let Some((adt_name, adt)) = book.adts.get_key_value(adt_name) {
          self.resugar_ctr_tagged_scott(book, adt, adt_name, errs);
        } else {
          bod.resugar_tagged_scott(book, errs);
        }
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => bod.resugar_tagged_scott(book, errs),

      Term::App { tag: Tag::Named(adt_name), fun, arg } => {
        if let Some((adt_name, adt)) = book.adts.get_key_value(adt_name) {
          self.resugar_match_tagged_scott(book, adt_name, adt, errs);
        } else {
          fun.resugar_tagged_scott(book, errs);
          arg.resugar_tagged_scott(book, errs);
        }
      }

      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.resugar_tagged_scott(book, errs);
        snd.resugar_tagged_scott(book, errs);
      }
      Term::Mat { matched, arms } => {
        matched.resugar_tagged_scott(book, errs);
        for (_, arm) in arms {
          arm.resugar_tagged_scott(book, errs);
        }
      }
      Term::Lst { .. } => unreachable!(),
      Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Var { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
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
  fn resugar_ctr_tagged_scott(
    &mut self,
    book: &Book,
    adt: &Adt,
    adt_name: &Name,
    errs: &mut Vec<ReadbackError>,
  ) {
    let mut app = &mut *self;
    let mut current_arm = None;

    for ctr in &adt.ctrs {
      match app {
        Term::Lam { tag: Tag::Named(tag), nam, bod } if tag == adt_name => {
          if let Some(nam) = nam {
            if current_arm.is_some() {
              errs.push(ReadbackError::InvalidAdt);
              return;
            }
            current_arm = Some((nam.clone(), ctr));
          }
          app = bod;
        }
        _ => {
          errs.push(ReadbackError::InvalidAdt);
          return;
        }
      }
    }

    let Some((arm_name, (ctr, ctr_args))) = current_arm else {
      errs.push(ReadbackError::InvalidAdt);
      return;
    };

    let mut cur = &mut *app;

    for field in ctr_args.iter().rev() {
      let expected_tag = Tag::adt_field(adt_name, ctr, field);

      match cur {
        Term::App { tag, .. } if tag == &expected_tag => {
          let Term::App { tag, fun, .. } = cur else { unreachable!() };
          *tag = Tag::Static;
          cur = fun.as_mut();
        }
        Term::App { tag, .. } => {
          errs.push(ReadbackError::UnexpectedTag(expected_tag, tag.clone()));
          return;
        }
        _ => {
          errs.push(ReadbackError::InvalidAdt);
          return;
        }
      }
    }

    match cur {
      Term::Var { nam } if nam == &arm_name => {}
      _ => {
        errs.push(ReadbackError::InvalidAdt);
        return;
      }
    }

    *cur = Term::r#ref(ctr);
    *self = std::mem::take(app);

    self.resugar_tagged_scott(book, errs);
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
  fn resugar_match_tagged_scott(
    &mut self,
    book: &Book,
    adt_name: &Name,
    adt: &Adt,
    errs: &mut Vec<ReadbackError>,
  ) {
    let mut cur = &mut *self;
    let mut arms = Vec::new();

    for (ctr, ctr_args) in adt.ctrs.iter().rev() {
      match cur {
        Term::App { tag: Tag::Named(tag), fun, arg } if tag == adt_name => {
          let mut args = Vec::new();
          let mut arm = arg.as_mut();

          for field in ctr_args {
            let expected_tag = Tag::adt_field(adt_name, ctr, field);

            match arm {
              Term::Lam { tag, .. } if tag == &expected_tag => {
                let Term::Lam { nam, bod, .. } = arm else { unreachable!() };

                args.push(nam.clone().map_or(Pattern::Var(None), |x| Pattern::Var(Some(x))));
                arm = bod.as_mut();
              }
              _ => {
                if let Term::Lam { tag, .. } = arm {
                  errs.push(ReadbackError::UnexpectedTag(expected_tag.clone(), tag.clone()));
                }

                let arg = Name::from(format!("{ctr}.{field}"));
                args.push(Pattern::Var(Some(arg.clone())));
                *arm = Term::tagged_app(expected_tag, std::mem::take(arm), Term::Var { nam: arg });
              }
            }
          }

          arms.push((Pattern::Ctr(ctr.clone(), args), arm));
          cur = &mut *fun;
        }
        _ => {
          errs.push(ReadbackError::InvalidAdtMatch);
          return;
        }
      }
    }

    let matched = Box::new(std::mem::take(cur));
    let arms = arms.into_iter().rev().map(|(pat, term)| (pat, std::mem::take(term))).collect();
    *self = Term::Mat { matched, arms };

    self.resugar_tagged_scott(book, errs);
  }
}
