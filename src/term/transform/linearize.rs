use crate::term::{Book, MatchNum, Name, Pattern, Tag, Term};
use hvmc::run::Val;
use std::collections::{hash_map::Entry, HashMap};

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
    let mut var_uses = HashMap::new();
    count_var_uses_in_term(self, &mut var_uses);
    term_to_affine(self, &mut var_uses, &mut HashMap::new(), &mut HashMap::new());
  }

  /// Returns false wether the term has no unscoped terms,
  /// or all its unscoped binds and usage pairs are within the term.
  fn has_unscoped(&self) -> bool {
    let (decl, uses) = self.unscoped_vars();
    decl.symmetric_difference(&uses).count() > 0
  }
}

fn count_var_uses_in_term(term: &Term, uses: &mut HashMap<Name, Val>) {
  match term {
    // Var users
    Term::Var { nam } => {
      *uses.entry(nam.clone()).or_default() += 1;
    }
    // Var producers
    Term::Lam { nam, bod, .. } => {
      add_var(nam.as_ref(), uses);
      count_var_uses_in_term(bod, uses);
    }
    Term::Dup { fst, snd, val, nxt, .. }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      add_var(fst.as_ref(), uses);
      add_var(snd.as_ref(), uses);
      count_var_uses_in_term(val, uses);
      count_var_uses_in_term(nxt, uses);
    }
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      add_var(nam.as_ref(), uses);
      count_var_uses_in_term(val, uses);
      count_var_uses_in_term(nxt, uses);
    }
    Term::Let { .. } => unreachable!(),
    // Others
    Term::Chn { bod, .. } => count_var_uses_in_term(bod, uses),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      count_var_uses_in_term(fst, uses);
      count_var_uses_in_term(snd, uses);
    }
    Term::Match { scrutinee, arms } => {
      count_var_uses_in_term(scrutinee, uses);
      for (rule, term) in arms {
        if let Pattern::Num(MatchNum::Succ(Some(nam))) = rule {
          add_var(nam.as_ref(), uses);
        }

        count_var_uses_in_term(term, uses);
      }
    }
    Term::List { .. } => unreachable!("Should have been desugared already"),
    Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => (),
  }
}

/// Var-declaring terms
fn term_with_bind_to_affine(
  term: &mut Term,
  nam: &mut Option<Name>,
  var_uses: &mut HashMap<Name, Val>,
  inst_count: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) {
  if let Some(name) = nam {
    if var_uses.contains_key(name) {
      term_to_affine(term, var_uses, inst_count, let_bodies);
      let instantiated_count = get_var_uses(Some(name), inst_count);
      duplicate_lam(nam, term, instantiated_count);
      return;
    }
  }

  term_to_affine(term, var_uses, inst_count, let_bodies);
}

fn term_to_affine(
  term: &mut Term,
  var_uses: &mut HashMap<Name, Val>,
  // Count to number of times a `Term::Var { nam }` has been reached without being linearized out
  inst_count: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) {
  match term {
    Term::Lam { nam, bod, .. } => term_with_bind_to_affine(bod, nam, var_uses, inst_count, let_bodies),

    Term::Let { pat: Pattern::Var(Some(nam)), val, nxt } => {
      let uses = var_uses[nam];
      match uses {
        0 => {
          if val.has_unscoped() {
            term_to_affine(val, var_uses, inst_count, let_bodies);
            term_to_affine(nxt, var_uses, inst_count, let_bodies);

            let Term::Let { val, nxt, .. } = std::mem::take(term) else { unreachable!() };
            *term = Term::Let { pat: Pattern::Var(None), val, nxt };

            return;
          }

          // We are going to remove the val term,
          // so we need to remove the free variables it uses from the vars count
          for (var, used) in val.free_vars() {
            let Entry::Occupied(mut entry) = var_uses.entry(var) else { unreachable!() };

            if *entry.get() <= used {
              entry.remove();
            } else {
              *entry.get_mut() -= used;
            }
          }

          term_to_affine(nxt, var_uses, inst_count, let_bodies);
        }
        1 => {
          term_to_affine(val, var_uses, inst_count, let_bodies);
          let_bodies.insert(nam.clone(), std::mem::take(val.as_mut()));
          term_to_affine(nxt, var_uses, inst_count, let_bodies);
        }
        uses => {
          term_to_affine(val, var_uses, inst_count, let_bodies);
          term_to_affine(nxt, var_uses, inst_count, let_bodies);

          let mut instantiated_count = get_var_uses(Some(nam), inst_count);

          if uses != instantiated_count {
            // TODO: This is done because the number of uses changed (because a term was linearized out)
            // It creates an extra half-erased-dup `let {nam_n *} = nam_m_dup; nxt` to match the correct labels.
            // Should be refactored.
            instantiated_count += 1;
          };

          duplicate_let(nam, nxt, instantiated_count, val);
        }
      }
      *term = std::mem::take(nxt.as_mut());
    }

    Term::Let { pat: Pattern::Var(None), val, nxt } => {
      if val.has_unscoped() {
        term_to_affine(val, var_uses, inst_count, let_bodies);
        term_to_affine(nxt, var_uses, inst_count, let_bodies);
      } else {
        let Term::Let { nxt, .. } = std::mem::take(term) else { unreachable!() };
        *term = *nxt;
      }
    }

    Term::Dup { fst, snd, val, nxt, .. }
    | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
      let uses_fst = get_var_uses(fst.as_ref(), var_uses);
      let uses_snd = get_var_uses(snd.as_ref(), var_uses);
      term_to_affine(val, var_uses, inst_count, let_bodies);
      term_to_affine(nxt, var_uses, inst_count, let_bodies);
      duplicate_lam(fst, nxt, uses_fst);
      duplicate_lam(snd, nxt, uses_snd);
    }

    Term::Let { .. } => unreachable!(),

    // Var-using terms
    Term::Var { nam } => {
      *var_uses.get_mut(nam).unwrap() -= 1;
      if let Some(subst) = let_bodies.remove(nam) {
        *term = subst;
      } else {
        let instantiated_count = inst_count.entry(nam.clone()).or_default();
        *instantiated_count += 1;

        *nam = dup_name(nam, *instantiated_count);
      }
    }

    // Others
    Term::Chn { bod, .. } => term_to_affine(bod, var_uses, inst_count, let_bodies),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      term_to_affine(fst, var_uses, inst_count, let_bodies);
      term_to_affine(snd, var_uses, inst_count, let_bodies);
    }
    Term::Match { scrutinee, arms } => {
      term_to_affine(scrutinee, var_uses, inst_count, let_bodies);
      for (rule, term) in arms {
        match rule {
          Pattern::Num(MatchNum::Succ(Some(nam))) => {
            term_with_bind_to_affine(term, nam, var_uses, inst_count, let_bodies);
          }
          Pattern::Num(_) => term_to_affine(term, var_uses, inst_count, let_bodies),
          _ => unreachable!(),
        }
      }
    }
    Term::List { .. } => unreachable!("Should have been desugared already"),
    Term::Era | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } => (),
  };
}

fn get_var_uses(nam: Option<&Name>, var_uses: &HashMap<Name, Val>) -> Val {
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
fn make_dup_tree(nam: &Name, nxt: &mut Term, uses: Val, mut dup_body: Option<&mut Term>) {
  let free_vars = &mut nxt.free_vars();

  if let Some(ref body) = dup_body {
    free_vars.extend(body.free_vars());
  };

  let make_name = |uses| {
    let dup_name = dup_name(nam, uses);
    free_vars.contains_key(&dup_name).then_some(dup_name)
  };

  for i in (1 .. uses).rev() {
    *nxt = Term::Dup {
      tag: Tag::Numeric(0),
      fst: make_name(i),
      snd: if i == uses - 1 { make_name(uses) } else { Some(internal_dup_name(nam, i)) },
      val: if i == 1 {
        Box::new(dup_body.as_deref_mut().map_or_else(|| Term::Var { nam: nam.clone() }, std::mem::take))
      } else {
        Box::new(Term::Var { nam: internal_dup_name(nam, i-1) })
      },
      nxt: Box::new(std::mem::take(nxt)),
    };
  }
}

fn duplicate_lam(nam: &mut Option<Name>, nxt: &mut Term, uses: Val) {
  match uses {
    0 => *nam = None,
    1 => *nam = Some(dup_name(nam.as_ref().unwrap(), 1)),
    uses => make_dup_tree(nam.as_ref().unwrap(), nxt, uses, None),
  }
}

fn duplicate_let(nam: &Name, nxt: &mut Term, uses: Val, let_body: &mut Term) {
  make_dup_tree(nam, nxt, uses, Some(let_body));
}

fn dup_name(nam: &Name, uses: Val) -> Name {
  Name(format!("{nam}_{uses}"))
}

fn internal_dup_name(nam: &Name, uses: Val) -> Name {
  Name(format!("{}_dup", dup_name(nam, uses)))
}

fn add_var(nam: Option<&Name>, uses: &mut HashMap<Name, Val>) {
  if let Some(nam) = nam {
    uses.entry(nam.clone()).or_insert(0);
  }
}
