use crate::term::{Book, Name, Pattern, Tag, Term};
use std::collections::HashMap;

/// Erases variables that weren't used, dups the ones that were used more than once.
/// Substitutes lets into their variable use.
/// In details:
/// For all var declarations:
///   If they're used 0 times: erase the declaration
///   If they're used 1 time: leave them as-is
///   If they're used more times: insert dups to make var use affine
/// For all let vars:
///   If they're used 0 times: Discard the let
///   If they're used 1 time: substitute the body in the var use
///   If they're use more times: add dups for all the uses, put the let body at the root dup.
/// Precondition: All variables are bound and have unique names within each definition.
impl Book {
  pub fn linearize_vars(&mut self) {
    for def in self.defs.values_mut() {
      def.rule_mut().body.linearize_vars();
    }
  }
}

impl Term {
  pub fn linearize_vars(&mut self) {
    term_to_affine(self, &mut HashMap::new());
  }

  /// Returns false whether the term has no unscoped terms,
  /// or all its unscoped binds and usage pairs are within the term.
  fn has_unscoped(&self) -> bool {
    let (decl, uses) = self.unscoped_vars();
    decl.symmetric_difference(&uses).count() > 0
  }
}

fn term_to_affine(term: &mut Term, var_uses: &mut HashMap<Name, u64>) {
  match term {
    Term::Let { pat: Pattern::Var(Some(nam)), val, nxt } => {
      term_to_affine(nxt, var_uses);

      match get_var_uses(Some(nam), var_uses) {
        0 => {
          if val.has_unscoped() {
            term_to_affine(val, var_uses);

            let Term::Let { val, nxt, .. } = term else { unreachable!() };
            let val = std::mem::take(val);
            let nxt = std::mem::take(nxt);

            *term = Term::Let { pat: Pattern::Var(None), val, nxt };
          } else {
            *term = std::mem::take(nxt.as_mut());
          }
        }
        1 => {
          term_to_affine(val, var_uses);
          nxt.subst(nam, val.as_ref());
          *term = std::mem::take(nxt.as_mut());
        }
        instantiated_count => {
          term_to_affine(val, var_uses);
          duplicate_term(nam, nxt, instantiated_count, Some(val));
          *term = std::mem::take(nxt.as_mut());
        }
      }
    }

    Term::Let { pat: Pattern::Var(None), val, nxt } => {
      term_to_affine(nxt, var_uses);

      if val.has_unscoped() {
        term_to_affine(val, var_uses);
      } else {
        let Term::Let { nxt, .. } = term else { unreachable!() };
        let nxt = std::mem::take(nxt.as_mut());
        *term = nxt;
      }
    }

    Term::Lam { nam, bod, .. } => term_with_binds_to_affine(bod, [nam], var_uses),
    Term::Dup { bnd, val, nxt, .. } => {
      term_to_affine(val, var_uses);
      term_with_binds_to_affine(nxt, bnd, var_uses);
    }
    Term::Let { pat, val, nxt } => {
      term_to_affine(val, var_uses);
      term_with_binds_to_affine(nxt, pat.bind_or_eras_mut(), var_uses);
    }

    // Var-using terms
    Term::Var { nam } => {
      let instantiated_count = var_uses.entry(nam.clone()).or_default();
      *instantiated_count += 1;

      *nam = dup_name(nam, *instantiated_count);
    }

    // Others
    Term::Chn { bod, .. } => term_to_affine(bod, var_uses),
    Term::App { fun: fst, arg: snd, .. } | Term::Opx { fst, snd, .. } => {
      term_to_affine(fst, var_uses);
      term_to_affine(snd, var_uses);
    }
    Term::Mat { args, rules } => {
      for arg in args {
        term_to_affine(arg, var_uses);
      }
      for rule in rules {
        let nams = rule.pats.iter_mut().flat_map(|p| p.bind_or_eras_mut());
        term_with_binds_to_affine(&mut rule.body, nams, var_uses)
      }
    }
    Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
      for el in els {
        term_to_affine(el, var_uses);
      }
    }

    Term::Era | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Err => {}
  };
}

/// Var-declaring terms
fn term_with_binds_to_affine<'a>(
  term: &mut Term,
  nams: impl IntoIterator<Item = &'a mut Option<Name>>,
  var_uses: &mut HashMap<Name, u64>,
) {
  term_to_affine(term, var_uses);

  for nam in nams {
    if nam.is_some() {
      let instantiated_count = get_var_uses(nam.as_ref(), var_uses);
      duplicate_bind(nam, term, instantiated_count);
    }
  }
}

fn get_var_uses(nam: Option<&Name>, var_uses: &HashMap<Name, u64>) -> u64 {
  nam.and_then(|nam| var_uses.get(nam).copied()).unwrap_or_default()
}

/// Creates the duplication bindings for variables used multiple times.
///
/// Example:
///
/// `@x (x x x x)` becomes `@x let {x0 x1 x2 x3} = x; (x0 x1 x2 x3)`.
///
/// `let x = @y y; (x x x)` becomes `let {x0 x1 x2} = @y y; (x0 x1 x2)`.
fn duplicate_term(nam: &Name, nxt: &mut Term, uses: u64, dup_body: Option<&mut Term>) {
  debug_assert!(uses > 1);

  *nxt = Term::Dup {
    tag: Tag::Auto,
    bnd: (1 .. uses + 1).map(|i| Some(dup_name(nam, i))).collect(),
    val: Box::new(dup_body.map_or_else(|| Term::Var { nam: nam.clone() }, std::mem::take)),
    nxt: Box::new(std::mem::take(nxt)),
  }
}

fn duplicate_bind(nam: &mut Option<Name>, nxt: &mut Term, uses: u64) {
  match uses {
    0 => *nam = None,
    1 => *nam = Some(dup_name(nam.as_ref().unwrap(), 1)),
    uses => duplicate_term(nam.as_ref().unwrap(), nxt, uses, None),
  }
}

fn dup_name(nam: &Name, uses: u64) -> Name {
  if uses == 1 { nam.clone() } else { Name::new(format!("{nam}_{uses}")) }
}
