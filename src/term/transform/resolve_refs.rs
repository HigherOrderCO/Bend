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

        for name in rule.pats.iter().flat_map(Pattern::names) {
          push_scope(Some(name), &mut scope);
        }

        rule.body.resolve_refs(&self.def_names, &mut scope)?;
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn resolve_refs<'a>(
    &'a mut self,
    def_names: &DefNames,
    scope: &mut HashMap<&'a Name, usize>,
  ) -> Result<(), String> {
    match self {
      Term::Lam { nam, bod, .. } => {
        push_scope(nam.as_ref(), scope);
        bod.resolve_refs(def_names, scope)?;
        pop_scope(nam.as_ref(), scope);
      }
      Term::Let { pat: Pattern::Var(nam), val, nxt } => {
        val.resolve_refs(def_names, scope)?;
        push_scope(nam.as_ref(), scope);
        nxt.resolve_refs(def_names, scope)?;
        pop_scope(nam.as_ref(), scope);
      }
      Term::Let { pat, val, nxt } => {
        val.resolve_refs(def_names, scope)?;

        for nam in pat.names() {
          push_scope(Some(nam), scope);
        }

        nxt.resolve_refs(def_names, scope)?;

        for nam in pat.names() {
          pop_scope(Some(nam), scope);
        }
      }
      Term::Dup { tag: _, fst, snd, val, nxt } => {
        val.resolve_refs(def_names, scope)?;
        push_scope(fst.as_ref(), scope);
        push_scope(snd.as_ref(), scope);
        nxt.resolve_refs(def_names, scope)?;
        pop_scope(fst.as_ref(), scope);
        pop_scope(snd.as_ref(), scope);
      }

      // If variable not defined, we check if it's a ref and swap if it is.
      Term::Var { nam } => {
        if is_var_in_scope(nam, scope) {
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
          let nam = if let Pattern::Num(MatchNum::Succ(Some(nam))) = pat { nam.as_ref() } else { None };
          push_scope(nam, scope);

          term.resolve_refs(def_names, scope)?;

          pop_scope(nam, scope);
        }
      }
      Term::List { .. } => unreachable!("Should have been desugared already"),
      Term::Lnk { .. }
      | Term::Ref { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::Era
      | Term::Invalid => (),
    }
    Ok(())
  }
}

fn push_scope<'a>(name: Option<&'a Name>, scope: &mut HashMap<&'a Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name).or_default();
    *var_scope += 1;
  }
}

fn pop_scope<'a>(name: Option<&'a Name>, scope: &mut HashMap<&'a Name, usize>) {
  if let Some(name) = name {
    let var_scope = scope.entry(name).or_default();
    *var_scope -= 1;
  }
}

fn is_var_in_scope<'a>(name: &'a Name, scope: &HashMap<&'a Name, usize>) -> bool {
  match scope.get(name) {
    Some(entry) => *entry == 0,
    None => true,
  }
}
