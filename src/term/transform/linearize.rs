use crate::term::{Book, LetPat, MatchNum, Name, RulePat, Term};
use hvmc::run::Val;
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
      for rule in def.rules.iter_mut() {
        rule.body.linearize_vars();
      }
    }
  }
}

impl Term {
  pub fn linearize_vars(&mut self) {
    let mut var_uses = HashMap::new();
    count_var_uses_in_term(self, &mut var_uses);
    term_to_affine(self, &mut var_uses, &mut HashMap::new());
  }
}

fn count_var_uses_in_term(term: &Term, uses: &mut HashMap<Name, Val>) {
  match term {
    Term::Var { nam } => {
      *uses.entry(nam.clone()).or_default() += 1;
    }
    Term::Lam { nam, bod } => {
      add_var(nam.as_ref(), uses);
      count_var_uses_in_term(bod, uses)
    }
    Term::Dup { fst, snd, val, nxt, .. } => {
      add_var(fst.as_ref(), uses);
      add_var(snd.as_ref(), uses);
      count_var_uses_in_term(val, uses);
      count_var_uses_in_term(nxt, uses);
    }
    Term::Let { pat: LetPat::Var(nam), val, nxt } => {
      add_var(Some(nam), uses);
      count_var_uses_in_term(val, uses);
      count_var_uses_in_term(nxt, uses);
    }
    Term::Let { pat: LetPat::Tup(fst, snd), val, nxt } => {
      add_var(fst.as_ref(), uses);
      add_var(snd.as_ref(), uses);
      count_var_uses_in_term(val, uses);
      count_var_uses_in_term(nxt, uses);
    }
    Term::Chn { bod, .. } => count_var_uses_in_term(bod, uses),
    Term::App { fun, arg } => {
      count_var_uses_in_term(fun, uses);
      count_var_uses_in_term(arg, uses);
    }
    Term::Sup { fst, snd } | Term::Tup { fst, snd } | Term::Opx { fst, snd, .. } => {
      count_var_uses_in_term(fst, uses);
      count_var_uses_in_term(snd, uses);
    }
    Term::Match { scrutinee, arms } => {
      count_var_uses_in_term(scrutinee, uses);
      for (rule, term) in arms {
        if let RulePat::Num(MatchNum::Succ(nam)) = rule {
          add_var(nam.as_ref(), uses)
        }

        count_var_uses_in_term(term, uses);
      }
    }
    Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Era => (),
  }
}

fn term_to_affine(term: &mut Term, var_uses: &mut HashMap<Name, Val>, let_bodies: &mut HashMap<Name, Term>) {
  match term {
    // Var-declaring terms
    Term::Lam { nam, bod } => {
      if let Some(nam_some) = nam {
        if let Some(uses) = var_uses.get(nam_some).copied() {
          term_to_affine(bod, var_uses, let_bodies);
          duplicate_lam(nam, bod, uses);
        } else {
          term_to_affine(bod, var_uses, let_bodies);
        }
      } else {
        term_to_affine(bod, var_uses, let_bodies);
      }
    }

    Term::Let { pat: LetPat::Var(nam), val, nxt } => {
      let uses = var_uses[nam];
      match uses {
        0 => {
          term_to_affine(nxt, var_uses, let_bodies);
        }
        1 => {
          term_to_affine(val, var_uses, let_bodies);
          let_bodies.insert(nam.clone(), std::mem::replace(val.as_mut(), Term::Era));
          term_to_affine(nxt, var_uses, let_bodies);
        }
        uses => {
          term_to_affine(val, var_uses, let_bodies);
          term_to_affine(nxt, var_uses, let_bodies);
          duplicate_let(nam, nxt, uses, val);
        }
      }
      *term = std::mem::replace(nxt.as_mut(), Term::Era);
    }

    Term::Dup { fst, snd, val, nxt, .. } | Term::Let { pat: LetPat::Tup(fst, snd), val, nxt } => {
      let uses_fst = get_var_uses(fst.as_ref(), var_uses);
      let uses_snd = get_var_uses(snd.as_ref(), var_uses);
      term_to_affine(val, var_uses, let_bodies);
      term_to_affine(nxt, var_uses, let_bodies);
      duplicate_lam(fst, nxt, uses_fst);
      duplicate_lam(snd, nxt, uses_snd);
    }

    // Var-using terms
    Term::Var { nam } => {
      let uses = var_uses[nam];
      *var_uses.get_mut(nam).unwrap() -= 1;
      if let Some(subst) = let_bodies.remove(nam) {
        *term = subst.clone();
      } else {
        *nam = dup_name(nam, uses);
      }
    }

    // Others
    Term::Chn { bod, .. } => term_to_affine(bod, var_uses, let_bodies),
    Term::App { fun, arg } => {
      term_to_affine(fun, var_uses, let_bodies);
      term_to_affine(arg, var_uses, let_bodies);
    }
    Term::Sup { fst, snd } | Term::Tup { fst, snd } | Term::Opx { fst, snd, .. } => {
      term_to_affine(fst, var_uses, let_bodies);
      term_to_affine(snd, var_uses, let_bodies);
    }
    Term::Match { scrutinee, arms } => {
      term_to_affine(scrutinee, var_uses, let_bodies);
      for (rule, term) in arms {
        let RulePat::Num(num) = rule else { unreachable!() };

        if let MatchNum::Succ(nam) = num {
          if let Some(nam_some) = nam {
            if let Some(uses) = var_uses.get(nam_some).copied() {
              term_to_affine(term, var_uses, let_bodies);
              duplicate_lam(nam, term, uses);
              continue;
            }
          }
        }

        term_to_affine(term, var_uses, let_bodies)
      }
    }
    Term::Era | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } => (),
  };
}

fn get_var_uses(nam: Option<&Name>, var_uses: &HashMap<Name, Val>) -> Val {
  if let Some(nam) = nam { *var_uses.get(nam).unwrap() } else { 0 }
}

fn make_dup_tree(nam: &Name, nxt: &mut Term, uses: Val, dup_body: Option<&mut Term>) {
  // TODO: Is there a difference between a list of dups and a complete binary tree of dups
  // Creates this: "dup x1 x1_dup = body; dup x2 x2_dup = x1_dup; dup x3 x4 = x2_dup; nxt"
  for i in (1 .. uses).rev() {
    let old_nxt = std::mem::replace(nxt, Term::Era);
    *nxt = Term::Dup {
      tag: None,
      fst: Some(dup_name(nam, i)),
      snd: if i == uses - 1 { Some(dup_name(nam, uses)) } else { Some(internal_dup_name(nam, uses)) },
      val: if i == 1 {
        if let Some(dup_body) = &dup_body {
          Box::new((*dup_body).clone()) // TODO: don't clone here
        } else {
          Box::new(Term::Var { nam: nam.clone() })
        }
      } else {
        Box::new(Term::Var { nam: internal_dup_name(nam, uses) })
      },
      nxt: Box::new(old_nxt),
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
  make_dup_tree(nam, nxt, uses, Some(let_body))
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
