use crate::{
  maybe_grow,
  term::{Book, Pattern, Term},
};

impl Book {
  pub fn apply_bnd(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.apply_bnd();
      }
    }
  }
}

impl Term {
  pub fn apply_bnd(&mut self) {
    maybe_grow(|| {
      for children in self.children_mut() {
        children.apply_bnd();
      }
    });

    if let Term::Bnd { fun, ask, val, nxt } = self {
      let mut fvs = nxt.free_vars();
      ask.binds().flatten().for_each(|bind| _ = fvs.remove(bind));
      let fvs = fvs.into_keys().collect::<Vec<_>>();
      let nxt =
        fvs.iter().fold(*nxt.clone(), |nxt, nam| Term::lam(Pattern::Var(Some(nam.clone())), nxt.clone()));
      let nxt = Term::lam(*ask.clone(), nxt);
      let term = Term::call(Term::Ref { nam: fun.clone() }, [*val.clone(), nxt]);
      *self = Term::call(term, fvs.into_iter().map(|nam| Term::Var { nam }));
    }
  }
}
