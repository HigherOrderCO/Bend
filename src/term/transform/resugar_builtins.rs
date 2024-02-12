use crate::term::{Term, LCONS, LNIL, SCONS, SNIL};

impl Term {
  pub fn resugar_builtins(&mut self) {
    self.resugar_strings();
    self.resugar_lists();
  }

  /// Rebuilds the String syntax sugar, converting `(SCons 97 SNil)` into `"a"`.
  pub fn resugar_strings(&mut self) {
    match self {
      // (SCons Num tail)
      Term::App {
        fun: box Term::App { fun: box Term::Ref { nam: ctr }, arg: box head, .. },
        arg: box tail,
        ..
      } => {
        head.resugar_strings();
        tail.resugar_strings();
        let head = std::mem::take(head);
        let mut tail = std::mem::take(tail);

        if ctr.as_str() == SCONS
          && let Term::Num { val } = head
          && let Term::Str { val: tail } = tail
        {
          // If well formed string, add the next character to the string we're building
          let head = unsafe { char::from_u32_unchecked(val as u32) }.to_string();
          let str = head + &tail;
          *self = Term::Str { val: str }
        } else {
          // Otherwise rebuild the constructor with the new tail

          // Create `(SCons head SNil)` instead of `(SCons head "")`
          if tail == (Term::Str { val: String::new() }) {
            tail = Term::r#ref(SNIL);
          }
          *self = Term::call(Term::Ref { nam: ctr.clone() }, [head, tail]);
        }
      }
      // (SNil)
      Term::Ref { nam: def_name } if def_name.as_str() == SNIL => *self = Term::Str { val: String::new() },

      Term::Mat { matched: scrutinee, arms } => {
        scrutinee.resugar_strings();
        for (_, arm) in arms {
          arm.resugar_strings();
        }
      }
      Term::Lst { els } => {
        for el in els {
          el.resugar_strings();
        }
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.resugar_strings();
        snd.resugar_strings();
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.resugar_strings();
      }
      Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Var { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
  }

  /// Rebuilds the List syntax sugar, converting `(LCons head LNil)` into `[head]`.
  pub fn resugar_lists(&mut self) {
    match self {
      // (LCons el tail)
      Term::App {
        fun: box Term::App { fun: box Term::Ref { nam: ctr }, arg: box head, .. },
        arg: box tail,
        ..
      } => {
        head.resugar_lists();
        tail.resugar_lists();
        let head = std::mem::take(head);
        let tail = std::mem::take(tail);

        if ctr.as_str() == LCONS
          && let Term::Lst { els: tail } = tail
        {
          // If well formed list, cons the next element to the list being formed
          let mut els = vec![head];
          els.extend(tail);
          *self = Term::Lst { els };
        } else {
          *self = Term::call(Term::Ref { nam: ctr.clone() }, [head, tail]);
        }
      }
      // (LNil)
      Term::Ref { nam: def_name } if def_name.as_str() == LNIL => *self = Term::Lst { els: vec![] },

      Term::Mat { matched, arms } => {
        matched.resugar_lists();
        for (_, arm) in arms {
          arm.resugar_lists();
        }
      }
      Term::Lst { els } => {
        for el in els {
          el.resugar_lists();
        }
      }
      Term::App { fun: fst, arg: snd, .. }
      | Term::Let { val: fst, nxt: snd, .. }
      | Term::Tup { fst, snd }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Opx { fst, snd, .. } => {
        fst.resugar_lists();
        snd.resugar_lists();
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.resugar_lists();
      }
      Term::Lnk { .. }
      | Term::Num { .. }
      | Term::Var { .. }
      | Term::Str { .. }
      | Term::Ref { .. }
      | Term::Era
      | Term::Err => {}
    }
  }
}
