use crate::term::{DefinitionBook, Name, Term};
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
impl DefinitionBook {
  pub fn linearize_vars(&mut self) -> anyhow::Result<()> {
    for (_, def) in &mut self.defs {
      def.body.linearize_vars()?;
    }
    Ok(())
  }
}

impl Term {
  pub fn linearize_vars(&mut self) -> anyhow::Result<()> {
    let mut var_uses = HashMap::new();
    get_var_use(self, &mut var_uses);
    *self = term_to_affine(self.clone(), &mut var_uses, &mut HashMap::new())?;
    Ok(())
  }
}

fn term_to_affine(
  term: Term,
  var_uses: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) -> anyhow::Result<Term> {
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
    Term::Match { cond, zero, succ } => {
      let cond = term_to_affine(*cond, var_uses, let_bodies)?;
      let zero = term_to_affine(*zero, var_uses, let_bodies)?;
      let succ = term_to_affine(*succ, var_uses, let_bodies)?;
      Term::Match { cond: Box::new(cond), zero: Box::new(zero), succ: Box::new(succ) }
    }
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

fn get_var_use(term: &Term, uses: &mut HashMap<Name, Val>) {
  match term {
    Term::Var { nam } => {
      *uses.entry(nam.clone()).or_default() += 1;
    }
    Term::Lam { nam, bod } => {
      if let Some(nam) = nam {
        if !uses.contains_key(nam) {
          uses.insert(nam.clone(), 0);
        }
      }
      get_var_use(bod, uses)
    }
    Term::Dup { fst, snd, val, nxt } => {
      if let Some(fst) = fst {
        if !uses.contains_key(fst) {
          uses.insert(fst.clone(), 0);
        }
      }
      if let Some(snd) = snd {
        if !uses.contains_key(snd) {
          uses.insert(snd.clone(), 0);
        }
      }
      get_var_use(val, uses);
      get_var_use(nxt, uses);
    }
    Term::Chn { bod, .. } => get_var_use(bod, uses),
    Term::Lnk { .. } => (),
    Term::Let { nam, val, nxt } => {
      if !uses.contains_key(nam) {
        uses.insert(nam.clone(), 0);
      }
      get_var_use(val, uses);
      get_var_use(nxt, uses);
    }
    Term::Ref { .. } => (),
    Term::App { fun, arg } => {
      get_var_use(fun, uses);
      get_var_use(arg, uses);
    }
    Term::Match { cond, zero, succ } => {
      get_var_use(cond, uses);
      get_var_use(zero, uses);
      get_var_use(succ, uses);
    }
    Term::Sup { fst, snd } => {
      get_var_use(fst, uses);
      get_var_use(snd, uses);
    }
    Term::Era => (),
    Term::Num { .. } => (),
    Term::Opx { fst, snd, .. } => {
      get_var_use(fst, uses);
      get_var_use(snd, uses);
    }
  }
}
