use crate::term::{Book, MatchNum, Name, Pattern, Tag, Term};
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

  /// Returns false wether the term has no unscoped terms,
  /// or all its unscoped binds and usage pairs are within the term.
  fn has_unscoped(&self) -> bool {
    let (decl, uses) = self.unscoped_vars();
    decl.symmetric_difference(&uses).count() > 0
  }
}

/// Var-declaring terms
fn term_with_bind_to_affine(term: &mut Term, nam: &mut Option<Name>, inst_count: &mut HashMap<Name, u64>) {
  term_to_affine(term, inst_count);

  if nam.is_some() {
    let instantiated_count = get_var_uses(nam.as_ref(), inst_count);
    duplicate_lam(nam, term, instantiated_count);
  }
}

fn term_to_affine(term: &mut Term, inst_count: &mut HashMap<Name, u64>) {
  match term {
    Term::Lam { nam, bod, .. } => term_with_bind_to_affine(bod, nam, inst_count),

    Term::Let { pat: Pattern::Var(Some(nam)), val, nxt } => {
      term_to_affine(nxt, inst_count);

      match get_var_uses(Some(nam), inst_count) {
        0 => {
          if val.has_unscoped() {
            term_to_affine(val, inst_count);

            let Term::Let { val, nxt, .. } = std::mem::take(term) else { unreachable!() };
            *term = Term::Let { pat: Pattern::Var(None), val, nxt };

            return;
          }
        }
        1 => {
          term_to_affine(val, inst_count);
          nxt.subst(&dup_name(nam, 1), val.as_ref());
        }
        instantiated_count => {
          term_to_affine(val, inst_count);
          duplicate_let(nam, nxt, instantiated_count, val);
        }
      }
      *term = std::mem::take(nxt.as_mut());
    }

    Term::Let { pat: Pattern::Var(None), val, nxt } => {
      term_to_affine(nxt, inst_count);

      if val.has_unscoped() {
        term_to_affine(val, inst_count);
      } else {
        let Term::Let { nxt, .. } = std::mem::take(term) else { unreachable!() };
        *term = *nxt;
      }
    }

    Term::Dup { fst, snd, val, nxt, .. }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      term_to_affine(val, inst_count);
      term_to_affine(nxt, inst_count);
      let uses_fst = get_var_uses(fst.as_ref(), inst_count);
      let uses_snd = get_var_uses(snd.as_ref(), inst_count);
      duplicate_lam(fst, nxt, uses_fst);
      duplicate_lam(snd, nxt, uses_snd);
    }

    Term::Let { .. } => unreachable!(),

    // Var-using terms
    Term::Var { nam } => {
      let instantiated_count = inst_count.entry(nam.clone()).or_default();
      *instantiated_count += 1;

      *nam = dup_name(nam, *instantiated_count);
    }

    // Others
    Term::Chn { bod, .. } => term_to_affine(bod, inst_count),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      term_to_affine(fst, inst_count);
      term_to_affine(snd, inst_count);
    }

    Term::Mat { matched, arms } => {
      term_to_affine(matched, inst_count);
      for (rule, term) in arms {
        match rule {
          Pattern::Num(MatchNum::Succ(Some(nam))) => {
            term_with_bind_to_affine(term, nam, inst_count);
          }
          Pattern::Num(_) => term_to_affine(term, inst_count),
          _ => unreachable!(),
        }
      }
    }

    Term::Lst { .. } => unreachable!("Should have been desugared already"),
    Term::Era | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Err => {}
  };
}

fn get_var_uses(nam: Option<&Name>, var_uses: &HashMap<Name, u64>) -> u64 {
  nam.and_then(|nam| var_uses.get(nam).copied()).unwrap_or_default()
}

// TODO: Is there a difference between a list of dups and a complete binary tree of dups?
//
/// # Example
///
/// ```txt
/// let {x1 x1_dup} = body;
/// let {x2 x2_dup} = x1_dup;
/// let {x3 x4}     = x2_dup;
/// nxt
/// ```
fn make_dup_tree(nam: &Name, nxt: &mut Term, uses: u64, mut dup_body: Option<&mut Term>) {
  for i in (1 .. uses).rev() {
    *nxt = Term::Dup {
      tag: Tag::Auto,
      fst: Some(dup_name(nam, i)),
      snd: if i == uses - 1 { Some(dup_name(nam, uses)) } else { Some(internal_dup_name(nam, i)) },
      val: if i == 1 {
        Box::new(
          dup_body.as_deref_mut().map_or_else(|| Term::Var { nam: nam.clone() }, |x| std::mem::take(x)),
        )
      } else {
        Box::new(Term::Var { nam: internal_dup_name(nam, i - 1) })
      },
      nxt: Box::new(std::mem::take(nxt)),
    };
  }
}

fn duplicate_lam(nam: &mut Option<Name>, nxt: &mut Term, uses: u64) {
  match uses {
    0 => *nam = None,
    1 => *nam = Some(dup_name(nam.as_ref().unwrap(), 1)),
    uses => make_dup_tree(nam.as_ref().unwrap(), nxt, uses, None),
  }
}

fn duplicate_let(nam: &Name, nxt: &mut Term, uses: u64, let_body: &mut Term) {
  make_dup_tree(nam, nxt, uses, Some(let_body));
}

fn dup_name(nam: &Name, uses: u64) -> Name {
  if uses == 1 { nam.clone() } else { Name::new(format!("{nam}_{uses}")) }
}

fn internal_dup_name(nam: &Name, uses: u64) -> Name {
  Name::new(format!("{}_dup", dup_name(nam, uses)))
}
