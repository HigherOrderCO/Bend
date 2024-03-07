use crate::term::{Adts, Book, Constructors, Name, NumCtr, Pattern, Term};

impl Book {
  /// Converts implicit match binds into explicit ones, using the
  /// field names specified in the ADT declaration or the default
  /// names for builtin constructors.
  ///
  /// Example:
  /// ```hvm
  /// data MyList = (Cons h t) | Nil
  /// match x y {
  ///   0 Nil: (A)
  ///   0 Cons: (B y.h y.t)
  ///   1+ Nil: (C x-1)
  ///   1+p (Cons x xs): (D p x xs)
  /// }
  /// ```
  /// becomes
  /// ```hvm
  /// match x y {
  ///   0 Nil: (A)
  ///   0 (Cons y.h y.t): (B y.h y.t)
  ///   1+x-1 Nil: (C x-1)
  ///   1+p (Cons x xs): (D p x xs)
  /// }
  /// ```
  pub fn desugar_implicit_match_binds(&mut self) {
    for def in self.defs.values_mut() {
      for rule in &mut def.rules {
        rule.body.desugar_implicit_match_binds(&self.ctrs, &self.adts);
      }
    }
  }
}

impl Term {
  pub fn desugar_implicit_match_binds(&mut self, ctrs: &Constructors, adts: &Adts) {
    Term::recursive_call(move || {
      for child in self.children_mut() {
        child.desugar_implicit_match_binds(ctrs, adts);
      }

      if let Term::Mat { args, rules } = self {
        // Make all the matched terms variables
        let mut match_args = vec![];
        for arg in args.iter_mut() {
          if let Term::Var { nam } = arg {
            match_args.push((nam.clone(), None))
          } else {
            let nam = Name::new(format!("%matched_{}", match_args.len()));
            let arg = std::mem::replace(arg, Term::Var { nam: nam.clone() });
            match_args.push((nam, Some(arg)));
          }
        }

        // Make implicit match binds explicit
        for rule in rules.iter_mut() {
          for ((nam, _), pat) in match_args.iter().zip(rule.pats.iter_mut()) {
            match pat {
              Pattern::Var(_) => (),
              Pattern::Ctr(ctr_nam, pat_args) => {
                let adt = &adts[ctrs.get(ctr_nam).unwrap()];
                let ctr_args = adt.ctrs.get(ctr_nam).unwrap();
                if pat_args.is_empty() && !ctr_args.is_empty() {
                  // Implicit ctr args
                  *pat_args = ctr_args
                    .iter()
                    .map(|field| Pattern::Var(Some(Name::new(format!("{nam}.{field}")))))
                    .collect();
                }
              }
              Pattern::Num(NumCtr::Num(_)) => (),
              Pattern::Num(NumCtr::Succ(_, Some(_))) => (),
              Pattern::Num(NumCtr::Succ(n, p @ None)) => {
                // Implicit num arg
                *p = Some(Some(Name::new(format!("{nam}-{n}"))));
              }
              Pattern::Tup(..) => (),
              Pattern::Lst(..) => (),
              Pattern::Str(..) => (),
            }
          }
        }

        // Add the binds to the extracted term vars.
        *self = match_args.into_iter().rfold(std::mem::take(self), |nxt, (nam, val)| {
          if let Some(val) = val {
            // Non-Var term that was extracted.
            Term::Let { pat: Pattern::Var(Some(nam)), val: Box::new(val), nxt: Box::new(nxt) }
          } else {
            nxt
          }
        });
      }
    })
  }
}
