// Pass for inlining functions that are just a reference to another one.

use crate::{
  diagnostics::Info,
  term::{Ctx, Name, Term},
};
use std::{collections::BTreeMap, fmt::Display};

#[derive(Debug, Clone)]
pub struct CyclicDefErr;

impl Display for CyclicDefErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Definition is a reference to itself.")
  }
}

impl Ctx<'_> {
  // When we find a function that is simply directly calling another function,
  // substitutes all occurrences of that function to the one being called, avoiding the unnecessary redirect.
  // In case there is a long chain of ref-to-ref-to-ref, we substitute values by the last function in the chain.
  pub fn simplify_ref_to_ref(&mut self) -> Result<(), Info> {
    self.info.start_pass();

    let mut ref_map: BTreeMap<Name, Name> = BTreeMap::new();
    // Find to which defs we're mapping the ones that are just references.
    'outer: for def_name in self.book.defs.keys() {
      let mut ref_name = def_name;
      let mut is_ref_to_ref = false;
      while let Term::Ref { nam: next_ref } = &self.book.defs.get(ref_name).unwrap().rule().body {
        if next_ref == ref_name {
          self.info.def_error(def_name.clone(), CyclicDefErr);
          continue 'outer;
        }
        ref_name = next_ref;
        is_ref_to_ref = true;
      }
      if is_ref_to_ref {
        ref_map.insert(def_name.clone(), ref_name.clone());
      }
    }

    // Substitute all the occurrences of ref-to-ref.
    for body in self.book.defs.values_mut().map(|def| &mut def.rule_mut().body) {
      subst_ref_to_ref(body, &ref_map);
    }

    self.info.fatal(())
  }
}

/// Returns whether any substitution happened within the term or not
pub fn subst_ref_to_ref(term: &mut Term, ref_map: &BTreeMap<Name, Name>) -> bool {
  match term {
    Term::Ref { nam: def_name } => {
      if let Some(target_name) = ref_map.get(def_name) {
        *def_name = target_name.clone();
        true
      } else {
        false
      }
    }
    Term::Lam { bod, .. } | Term::Chn { bod, .. } => subst_ref_to_ref(bod, ref_map),
    Term::App { fun: fst, arg: snd, .. }
    | Term::Let { val: fst, nxt: snd, .. }
    | Term::Dup { val: fst, nxt: snd, .. }
    | Term::Opx { fst, snd, .. } => {
      let fst_subst = subst_ref_to_ref(fst, ref_map);
      let snd_subst = subst_ref_to_ref(snd, ref_map);
      fst_subst | snd_subst
    }
    Term::Mat { args, rules } => {
      let mut subst = false;
      for arg in args {
        subst |= subst_ref_to_ref(arg, ref_map);
      }
      for rule in rules {
        subst |= subst_ref_to_ref(&mut rule.body, ref_map);
      }
      subst
    }
    Term::Lst { els } | Term::Sup { els, .. } | Term::Tup { els } => {
      let mut subst = false;
      for e in els {
        subst |= subst_ref_to_ref(e, ref_map);
      }
      subst
    }
    Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era => false,
    Term::Err => unreachable!(),
  }
}
