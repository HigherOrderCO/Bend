use std::collections::HashMap;

use crate::term::{Book, DefNames, MatchNum, Name, Pattern, Term};

impl Book {
  /// Decides if names inside a term belong to a Var or to a Ref.
  /// Precondition: Refs are encoded as vars, Constructors are resolved.
  /// Postcondition: Refs are encoded as refs, with the correct def id.
  pub fn resolve_refs(&mut self) -> Result<(), String> {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        let mut scope = HashMap::new();
        rule.pats.iter().for_each(|pat| {
          pat.names().cloned().for_each(|name| {
            push_scope(Some(name), &mut scope);
          })
        });
        rule.body.resolve_refs(&self.def_names, &mut scope)?;
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn resolve_refs(
    &mut self,
    def_names: &DefNames,
    scope: &mut HashMap<Name, usize>,
  ) -> Result<(), String> {
    match self {
      Term::Lam { nam, bod, .. } => {
        push_scope(nam.clone(), scope);
        bod.resolve_refs(def_names, scope)?;
        pop_scope(nam.clone(), scope);
      }
      Term::Let { pat: Pattern::Var(nam), val, nxt } => {
        val.resolve_refs(def_names, scope)?;
        push_scope(nam.clone(), scope);
        nxt.resolve_refs(def_names, scope)?;
        pop_scope(nam.clone(), scope);
      }
      Term::Let { pat, val, nxt } => {
        val.resolve_refs(def_names, scope)?;

        for nam in pat.names() {
          push_scope(Some(nam.clone()), scope)
        }

        nxt.resolve_refs(def_names, scope)?;

        for nam in pat.names() {
          pop_scope(Some(nam.clone()), scope)
        }
      }
      Term::Dup { tag: _, fst, snd, val, nxt } => {
        val.resolve_refs(def_names, scope)?;
        push_scope(fst.clone(), scope);
        push_scope(snd.clone(), scope);
        nxt.resolve_refs(def_names, scope)?;
        pop_scope(fst.clone(), scope);
        pop_scope(snd.clone(), scope);
      }

      // If variable not defined, we check if it's a ref and swap if it is.
      Term::Var { nam } => {
        if is_var_in_scope(nam.clone(), scope) {
          if matches!(nam.0.as_ref(), DefNames::ENTRY_POINT | DefNames::HVM1_ENTRY_POINT) {
            return Err("Main definition can't be referenced inside the program".to_string());
          }

          if let Some(def_id) = def_names.def_id(nam) {
            *self = Term::Ref { def_id };
          }
        }
      }
      Term::Chn { bod, .. } => bod.resolve_refs(def_names, scope)?,
      Term::App { fun: fst, arg: snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd }
      | Term::Opx { fst, snd, .. } => {
        fst.resolve_refs(def_names, scope)?;
        snd.resolve_refs(def_names, scope)?;
      }
      Term::Match { scrutinee, arms } => {
        scrutinee.resolve_refs(def_names, scope)?;
        for (pat, term) in arms {
          if let Pattern::Num(MatchNum::Succ(Some(nam))) = pat {
            push_scope(nam.clone(), scope)
          }

          term.resolve_refs(def_names, scope)?;

          if let Pattern::Num(MatchNum::Succ(Some(nam))) = pat {
            pop_scope(nam.clone(), scope)
          }
        }
      }
      Term::List { .. } => unreachable!("Should have been desugared already"),
      Term::Lnk { .. } | Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => (),
    }
    Ok(())
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
