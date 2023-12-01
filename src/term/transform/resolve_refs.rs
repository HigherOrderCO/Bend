use std::collections::HashMap;

use crate::term::{Book, DefNames, MatchNum, Name, Pattern, Term};

impl Book {
  /// Decides if names inside a term belong to a Var or to a Ref.
  /// Precondition: Refs are encoded as vars.
  /// Postcondition: Refs are encoded as refs, with the correct def id.
  pub fn resolve_refs(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.resolve_refs(&self.def_names);
      }
    }
  }
}

impl Term {
  pub fn resolve_refs(&mut self, def_names: &DefNames) {
    resolve_refs(self, def_names, &mut HashMap::new())
  }
}

fn resolve_refs(term: &mut Term, def_names: &DefNames, scope: &mut HashMap<Name, usize>) {
  match term {
    Term::Lam { nam, bod, .. } => {
      push_scope(nam.clone(), scope);
      resolve_refs(bod, def_names, scope);
      pop_scope(nam.clone(), scope);
    }
    Term::Let { pat: Pattern::Var(nam), val, nxt } => {
      resolve_refs(val, def_names, scope);
      push_scope(nam.clone(), scope);
      resolve_refs(nxt, def_names, scope);
      pop_scope(nam.clone(), scope);
    }
    Term::Dup { tag: _, fst, snd, val, nxt } | Term::Let { pat: Pattern::Tup(fst, snd), val, nxt } => {
      resolve_refs(val, def_names, scope);
      push_scope(fst.clone(), scope);
      push_scope(snd.clone(), scope);
      resolve_refs(nxt, def_names, scope);
      pop_scope(fst.clone(), scope);
      pop_scope(snd.clone(), scope);
    }
    Term::Let { .. } => todo!(),

    // If variable not defined, we check if it's a ref and swap if it is.
    Term::Var { nam } => {
      if is_var_in_scope(nam.clone(), scope) {
        if let Some(def_id) = def_names.def_id(nam) {
          *term = Term::Ref { def_id };
        }
      }
    }
    Term::Chn { bod, .. } => resolve_refs(bod, def_names, scope),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Sup { fst, snd, .. }
    | Term::Tup { fst, snd }
    | Term::Opx { fst, snd, .. } => {
      resolve_refs(fst, def_names, scope);
      resolve_refs(snd, def_names, scope);
    }
    Term::Match { scrutinee, arms } => {
      resolve_refs(scrutinee, def_names, scope);
      for (pat, term) in arms {
        if let Pattern::Num(MatchNum::Succ(p)) = pat {
          push_scope(p.clone(), scope)
        }

        resolve_refs(term, def_names, scope);

        if let Pattern::Num(MatchNum::Succ(p)) = pat {
          pop_scope(p.clone(), scope)
        }
      }
    }
    Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Era => (),
  }
}

fn push_scope(name: Option<Name>, scope: &mut HashMap<Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name.clone()).or_default();
    *var_scope += 1;
  }
}

fn pop_scope(name: Option<Name>, scope: &mut HashMap<Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name.clone()).or_default();
    *var_scope -= 1;
  }
}

fn is_var_in_scope(name: Name, scope: &mut HashMap<Name, usize>) -> bool {
  *scope.entry(name.clone()).or_default() == 0
}
