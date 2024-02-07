use crate::term::{Book, DefName, Definition, Term};
use indexmap::IndexMap;
use std::collections::HashSet;

impl Book {
  pub fn inline(&mut self) {
    let mut inlineables = HashSet::new();
    for (def_name, def) in self.defs.iter() {
      def.assert_no_pattern_matching_rules();
      if def.rules[0].body.is_inlineable() {
        inlineables.insert(def_name.clone());
      }
    }

    let defs = self.defs.clone();
    for def in self.defs.values_mut() {
      def.rules[0].body.inline(&inlineables, &defs);
    }
  }
}

impl Term {
  fn inline(&mut self, inlineables: &HashSet<DefName>, defs: &IndexMap<DefName, Definition>) {
    match self {
      Term::Ref { def_name } => {
        if inlineables.contains(def_name) {
          *self = defs.get(def_name).unwrap().rules[0].body.clone();
        }
      }

      Term::Lam { bod, .. } => bod.inline(inlineables, defs),

      Term::App { fun: fst, arg: snd, .. }
      | Term::Dup { val: fst, nxt: snd, .. }
      | Term::Opx { fst, snd, .. }
      | Term::Sup { fst, snd, .. }
      | Term::Tup { fst, snd } => {
        fst.inline(inlineables, defs);
        snd.inline(inlineables, defs);
      }

      Term::Match { arms, .. } => {
        for (_, bod) in arms {
          bod.inline(inlineables, defs);
        }
      }

      Term::Var { .. }
      | Term::Chn { .. }
      | Term::Lnk { .. }
      | Term::Let { .. }
      | Term::Num { .. }
      | Term::Str { .. }
      | Term::List { .. }
      | Term::Era => {}

      Term::Invalid => unreachable!(),
    }
  }

  /// Terms that compile to a single node can be inlineable,
  /// like erasers, variables (at lambda scope), numbers,
  /// lambdas, tuples and superpositions.
  fn is_inlineable(&self) -> bool {
    fn go(term: &Term, scope: usize) -> bool {
      match term {
        Term::Era | Term::Var { .. } | Term::Num { .. } => scope.saturating_sub(1) == 0,
        Term::Lam { bod, .. } => go(bod, scope + 1),
        Term::Tup { fst, snd } | Term::Sup { fst, snd, .. } => go(fst, scope + 1) && go(snd, scope + 1),

        Term::Chn { .. } | Term::Lnk { .. } => false,
        Term::Str { .. } | Term::List { .. } => false,
        Term::Let { .. } => false,
        Term::App { .. } => false,
        Term::Dup { .. } => false,
        Term::Opx { .. } => false,
        Term::Match { .. } => false,
        Term::Ref { .. } => false,

        Term::Invalid => unreachable!(),
      }
    }
    go(self, 0)
  }
}
