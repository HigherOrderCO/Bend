use super::{
  net_to_term::{ReadbackError, Reader},
  transform::encode_strs::{SCONS, SNIL},
  Adt, Name, Pattern, Tag, Term,
};

impl<'a> Reader<'a> {
  pub fn resugar_adts(&mut self, term: &mut Term) {
    match term {
      Term::Lam { tag, bod, .. } if *tag == Tag::string() => *term = self.resugar_string(std::mem::take(bod)),
      Term::Lam { tag, bod, .. } if *tag == Tag::list() => *term = self.resugar_list(std::mem::take(bod)),

      Term::Lam { tag: Tag::Named(adt_name), bod, .. } | Term::Chn { tag: Tag::Named(adt_name), bod, .. } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          return self.resugar_adts(bod);
        };

        match self.resugar_adt_cons(std::mem::take(term), adt, adt_name) {
          Ok(adt) => *term = adt,
          Err(err) => return self.error(err),
        }

        self.resugar_adts(term);
      }

      Term::Lam { bod, .. } | Term::Chn { bod, .. } => self.resugar_adts(bod),

      Term::App { tag: Tag::Named(adt_name), fun, arg } => {
        let Some((adt_name, adt)) = self.book.adts.get_key_value(adt_name) else {
          self.resugar_adts(fun);
          self.resugar_adts(arg);
          return;
        };

        match self.resugar_adt_match(std::mem::take(term), adt_name, adt) {
          Ok(mtch) => *term = mtch,
          Err(err) => return self.error(err),
        };

        self.resugar_adts(term);
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

  fn resugar_adt_cons(&mut self, mut term: Term, adt: &Adt, adt_name: &Name) -> Result<Term, ReadbackError> {
    let mut current_arm = None;

    for ctr in &adt.ctrs {
      self.deref(&mut term);
      match term {
        Term::Lam { tag: Tag::Named(tag), nam, bod } if &tag == adt_name => {
          if let Some(nam) = nam {
            if current_arm.is_some() {
              return Err(ReadbackError::InvalidAdt);
            }
            current_arm = Some((nam.clone(), ctr))
          }
          term = *bod;
        }
        _ => return Err(ReadbackError::InvalidAdt),
      }
    }

    let Some((arm_name, (ctr, ctr_args))) = current_arm else {
      return Err(ReadbackError::InvalidAdt);
    };

    let mut cur = &mut term;

    for _ in ctr_args {
      self.deref(cur);
      match cur {
        Term::App { tag: Tag::Static, fun, .. } => cur = fun,
        Term::App { tag: tag @ Tag::Named(_), fun, .. } => {
          *tag = Tag::Static;
          cur = fun
        }
        _ => return Err(ReadbackError::InvalidAdt),
      }
    }

    match cur {
      Term::Var { nam } if nam == &arm_name => {}
      _ => return Err(ReadbackError::InvalidAdt),
    }

    *cur = self.book.def_names.get_ref(ctr);

    Ok(term)
  }

  fn resugar_adt_match(&mut self, mut term: Term, adt_name: &Name, adt: &Adt) -> Result<Term, ReadbackError> {
    let mut arms = Vec::new();

    for (ctr, ctr_args) in adt.ctrs.iter().rev() {
      self.deref(&mut term);
      match term {
        Term::App { tag: Tag::Named(tag), fun, arg } if &tag == adt_name => {
          let mut args = Vec::new();
          let mut arm = *arg;

          for _ in ctr_args {
            self.deref(&mut arm);

            if !matches!(arm, Term::Lam { tag: Tag::Static, .. }) {
              let nam = self.namegen.unique();
              arm = Term::named_lam(nam.clone(), Term::arg_call(arm, Some(nam)));
            }

            match arm {
              Term::Lam { nam, bod, .. } => {
                args.push(nam.clone().map_or(Pattern::Var(None), |x| Pattern::Var(Some(x))));
                arm = *bod;
              }
              _ => unreachable!(),
            }
          }

          arms.push((Pattern::Ctr(ctr.clone(), args), arm));
          term = *fun;
        }
        _ => return Err(ReadbackError::InvalidAdtMatch),
      }
    }

    let scrutinee = Box::new(term);
    let arms = arms.into_iter().rev().map(|(pat, term)| (pat, term)).collect();
    Ok(Term::Match { scrutinee, arms })
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
