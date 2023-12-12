use std::collections::BTreeMap;

use crate::term::{Book, DefId, DefNames, Definition, Name, Pattern, Rule, Tag, Term};

impl Term {
  pub fn simplify_let(
    &mut self,
    def_names: &mut DefNames,
    counter: &mut usize,
    new_defs: &mut BTreeMap<DefId, Definition>,
  ) {
    match self {
      Term::Let { pat: Pattern::Tup(..), .. } => {
        let Term::Let { pat, mut val, mut nxt } = std::mem::take(self) else { unreachable!() };
        val.simplify_let(def_names, counter, new_defs);
        nxt.simplify_let(def_names, counter, new_defs);
        let new_name = make_let_name(counter);
        let def_id = def_names.insert(new_name.clone());
        *counter += 1;
        *self = Term::App { tag: Tag::Static, fun: Box::new(Term::Var { nam: new_name }), arg: val.clone() };
        let rule = Rule { pats: vec![pat.clone()], body: *nxt };
        new_defs.insert(def_id, Definition { def_id, rules: vec![rule] });
      }
      Term::Let { val, nxt, .. } => {
        val.simplify_let(def_names, counter, new_defs);
        nxt.simplify_let(def_names, counter, new_defs);
      }
      Term::Lam { bod, .. } | Term::Chn { bod, .. } => {
        bod.simplify_let(def_names, counter, new_defs);
      }
      Term::App { fun, arg, .. } => {
        fun.simplify_let(def_names, counter, new_defs);
        arg.simplify_let(def_names, counter, new_defs);
      }
      Term::Dup { val, nxt, .. } => {
        val.simplify_let(def_names, counter, new_defs);
        nxt.simplify_let(def_names, counter, new_defs);
      }
      Term::Tup { fst, snd } | Term::Sup { fst, snd, .. } | Term::Opx { fst, snd, .. } => {
        fst.simplify_let(def_names, counter, new_defs);
        snd.simplify_let(def_names, counter, new_defs);
      }
      Term::Match { scrutinee: _, arms } => {
        for (_, bod) in arms {
          bod.simplify_let(def_names, counter, new_defs);
        }
      }
      Term::Var { .. } | Term::Lnk { .. } | Term::Num { .. } | Term::Ref { .. } | Term::Era => (),
    }
  }
}

fn make_let_name(counter: &usize) -> Name {
  Name(format!("let%{counter}"))
}

impl Book {
  pub fn simplify_lets(&mut self) {
    let mut new_defs = BTreeMap::new();
    let let_counter = &mut 0;
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.simplify_let(&mut self.def_names, let_counter, &mut new_defs);
      }
    }
    self.defs.extend(new_defs);
  }
}
