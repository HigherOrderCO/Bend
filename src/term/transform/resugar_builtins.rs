use crate::term::{Term, LCONS, LNIL, SCONS, SNIL};

impl Term {
  pub fn resugar_builtins(&mut self) {
    self.resugar_strings();
    self.resugar_lists();
  }

  /// Rebuilds the String syntax sugar, converting `(Cons 97 Nil)` into `"a"`.
  pub fn resugar_strings(&mut self) {
    match self {
      // (String.cons Num tail)
      Term::App {
        fun: box Term::App { fun: box Term::Ref { nam: ctr }, arg: box head, .. },
        arg: box tail,
        ..
      } => {
        head.resugar_strings();
        tail.resugar_strings();
        let head = std::mem::take(head);
        let mut tail = std::mem::take(tail);

        if ctr.as_ref() == SCONS
          && let Term::Num { val } = head
          && let Term::Str { val: tail } = tail
        {
          // If well formed string, add the next character to the string we're building
          let head = unsafe { char::from_u32_unchecked(val as u32) }.to_string();
          let str = head + &tail;
          *self = Term::Str { val: str }
        } else {
          // Otherwise rebuild the constructor with the new tail

          // Create `(Cons head Nil)` instead of `(Cons head "")`
          if tail == (Term::Str { val: String::new() }) {
            tail = Term::r#ref(SNIL);
          }
          *self = Term::call(Term::Ref { nam: ctr.clone() }, [head, tail]);
        }
      }
      // (String.nil)
      Term::Ref { nam: def_name } if def_name.as_ref() == SNIL => *self = Term::Str { val: String::new() },

      Term::Mat { matched, arms } => {
        matched.resugar_strings();
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

  /// Rebuilds the List syntax sugar, converting `(Cons head Nil)` into `[head]`.
  pub fn resugar_lists(&mut self) {
    match self {
      // (List.cons el tail)
      Term::App {
        fun: box Term::App { fun: box Term::Ref { nam: ctr }, arg: box head, .. },
        arg: box tail,
        ..
      } => {
        head.resugar_lists();
        tail.resugar_lists();
        let head = std::mem::take(head);
        let tail = std::mem::take(tail);

        if ctr.as_ref() == LCONS
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
      // (List.nil)
      Term::Ref { nam: def_name } if def_name.as_ref() == LNIL => *self = Term::Lst { els: vec![] },

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
