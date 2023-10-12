use crate::term::{Name, Term};
use hvmc::Val;
use std::collections::HashMap;

/// Erases variables that weren't used, dups the ones that were used more than once.
/// Substitutes lets into their variable use.
/// Precondition: all variables in the term have unique names
pub fn term_to_affine(
  term: Term,
  var_uses: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) -> anyhow::Result<Term> {
  fn make_dup_tree(nam: &Name, mut nxt: Term, uses: Val, dup_body: Option<Term>) -> Term {
    // TODO: Is there a difference between a list of dups and a complete binary tree of dups?
    // Creates this: "dup x1 x1_dup = body; dup x2 x2_dup = x1_dup; dup x3 x4 = x2_dup; nxt"
    for i in (1 .. uses).rev() {
      nxt = Term::Dup {
        fst: Some(dup_name(nam, i)),
        snd: if i == uses - 1 { Some(dup_name(nam, uses)) } else { Some(internal_dup_name(nam, uses)) },
        val: if i == 1 {
          if let Some(dup_body) = &dup_body {
            Box::new(dup_body.clone()) // TODO: don't clone here
          } else {
            Box::new(Term::Var { nam: nam.clone() })
          }
        } else {
          Box::new(Term::Var { nam: internal_dup_name(nam, uses) })
        },
        nxt: Box::new(nxt),
      };
    }
    nxt
  }

  fn duplicate_lam(nam: Name, nxt: Term, uses: Val) -> (Term, Option<Name>) {
    // TODO: Is there a difference between a list of dups and a complete binary tree of dups?
    match uses {
      0 => (nxt, None),
      1 => (nxt, Some(dup_name(&nam, 1))),
      uses => (make_dup_tree(&nam, nxt, uses, None), Some(nam)),
    }
  }

  fn duplicate_let(nam: &Name, nxt: Term, uses: Val, let_body: Term) -> Term {
    make_dup_tree(nam, nxt, uses, Some(let_body))
  }

  fn dup_name(nam: &Name, uses: Val) -> Name {
    Name(format!("{nam}_{uses}"))
  }

  fn internal_dup_name(nam: &Name, uses: Val) -> Name {
    Name(format!("{}_dup", dup_name(nam, uses)))
  }

  let term = match term {
    Term::Lam { nam: None, bod } => {
      Term::Lam { nam: None, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) }
    }
    Term::Lam { nam: Some(nam), bod } => {
      if let Some(uses) = var_uses.get(&nam).copied() {
        let bod = term_to_affine(*bod, var_uses, let_bodies)?;
        let (bod, nam) = duplicate_lam(nam, bod, uses);
        Term::Lam { nam, bod: Box::new(bod) }
      } else {
        Term::Lam { nam: None, bod }
      }
    }
    Term::Var { nam } => {
      let uses = var_uses[&nam];
      *var_uses.get_mut(&nam).unwrap() -= 1;
      if let Some(subst) = let_bodies.remove(&nam) { subst } else { Term::Var { nam: dup_name(&nam, uses) } }
    }
    Term::Chn { nam, bod } => Term::Chn { nam, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) },
    Term::Let { nam, val, nxt } => {
      let uses = var_uses[&nam];
      match uses {
        0 => term_to_affine(*nxt, var_uses, let_bodies)?,
        1 => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let_bodies.insert(nam, val);
          term_to_affine(*nxt, var_uses, let_bodies)?
        }
        uses => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
          duplicate_let(&nam, nxt, uses, val)
        }
      }
    }
    Term::Dup { fst, snd, val, nxt } => {
      let uses_fst = fst.as_ref().map(|fst| *var_uses.get(fst).unwrap()).unwrap_or(0);
      let uses_snd = snd.as_ref().map(|snd| *var_uses.get(snd).unwrap()).unwrap_or(0);
      let val = term_to_affine(*val, var_uses, let_bodies)?;
      let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
      let (nxt, fst) = if let Some(fst) = fst { duplicate_lam(fst, nxt, uses_fst) } else { (nxt, fst) };
      let (nxt, snd) = if let Some(snd) = snd { duplicate_lam(snd, nxt, uses_snd) } else { (nxt, snd) };
      Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
    }
    Term::If { cond, then, els_ } => Term::If {
      cond: Box::new(term_to_affine(*cond, var_uses, let_bodies)?),
      then: Box::new(term_to_affine(*then, var_uses, let_bodies)?),
      els_: Box::new(term_to_affine(*els_, var_uses, let_bodies)?),
    },
    Term::App { fun, arg } => Term::App {
      fun: Box::new(term_to_affine(*fun, var_uses, let_bodies)?),
      arg: Box::new(term_to_affine(*arg, var_uses, let_bodies)?),
    },
    Term::Sup { fst, snd } => Term::Sup {
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    Term::Opx { op, fst, snd } => Term::Opx {
      op,
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    t @ (Term::Era | Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. }) => t,
  };
  Ok(term)
}
