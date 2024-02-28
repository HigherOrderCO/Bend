use crate::term::{Adts, Book, Constructors, Name, NumCtr, Pattern, Term};

impl Book {
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
    let mut to_desugar = vec![self];

    while let Some(term) = to_desugar.pop() {
      match term {
        Term::Mat { args, rules } => {
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
          *term = match_args.into_iter().rev().fold(std::mem::take(term), |nxt, (nam, val)| {
            if let Some(val) = val {
              // Non-Var term that was extracted.
              Term::Let { pat: Pattern::Var(Some(nam)), val: Box::new(val), nxt: Box::new(nxt) }
            } else {
              nxt
            }
          });

          // Add the next values to check
          let mut term = term;
          while let Term::Let { nxt, .. } = term {
            term = nxt;
          }
          let Term::Mat { args: _, rules } = term else { unreachable!() };
          to_desugar.extend(rules.iter_mut().map(|r| &mut r.body));
        }
        Term::Sup { els, .. } | Term::Lst { els } | Term::Tup { els } => {
          for el in els {
            to_desugar.push(el);
          }
        }
        Term::Let { pat: Pattern::Var(_), val: fst, nxt: snd }
        | Term::App { fun: fst, arg: snd, .. }
        | Term::Dup { val: fst, nxt: snd, .. }
        | Term::Opx { fst, snd, .. } => {
          to_desugar.push(fst);
          to_desugar.push(snd);
        }
        Term::Lam { bod, .. } | Term::Chn { bod, .. } => to_desugar.push(bod),
        Term::Era
        | Term::Ref { .. }
        | Term::Num { .. }
        | Term::Str { .. }
        | Term::Lnk { .. }
        | Term::Var { .. }
        | Term::Err => (),
        Term::Let { pat: _, .. } => {
          unreachable!("Expected destructor let expressions to have been desugared already")
        }
      }
    }
  }
}
