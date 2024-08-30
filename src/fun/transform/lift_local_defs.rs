use std::collections::BTreeSet;

use indexmap::IndexMap;

use crate::{
  fun::{Book, Definition, Name, Pattern, Rule, Term},
  maybe_grow,
};

impl Book {
  pub fn lift_local_defs(&mut self) {
    let mut defs = IndexMap::new();
    for (name, def) in self.defs.iter_mut() {
      let mut gen = 0;
      for rule in def.rules.iter_mut() {
        rule.body.lift_local_defs(name, def.check, &mut defs, &mut gen);
      }
    }
    self.defs.extend(defs);
  }
}

impl Rule {
  pub fn binds(&self) -> impl DoubleEndedIterator<Item = &Option<Name>> + Clone {
    self.pats.iter().flat_map(Pattern::binds)
  }
}

impl Term {
  pub fn lift_local_defs(
    &mut self,
    parent: &Name,
    check: bool,
    defs: &mut IndexMap<Name, Definition>,
    gen: &mut usize,
  ) {
    maybe_grow(|| match self {
      Term::Def { def, nxt } => {
        let local_name = Name::new(format!("{}__local_{}_{}", parent, gen, def.name));
        for rule in def.rules.iter_mut() {
          rule.body.lift_local_defs(&local_name, check, defs, gen);
        }
        nxt.lift_local_defs(parent, check, defs, gen);
        *gen += 1;

        let inner_defs =
          defs.keys().filter(|name| name.starts_with(local_name.as_ref())).cloned().collect::<BTreeSet<_>>();
        let (r#use, fvs, mut rules) =
          gen_use(inner_defs, &local_name, &def.name, nxt, std::mem::take(&mut def.rules));
        let source = std::mem::take(&mut def.source);
        *self = r#use;

        apply_closure(&mut rules, &fvs);

        let new_def = Definition::new_gen(local_name.clone(), rules, source, check);
        defs.insert(local_name.clone(), new_def);
      }
      _ => {
        for child in self.children_mut() {
          child.lift_local_defs(parent, check, defs, gen);
        }
      }
    })
  }
}

fn gen_use(
  inner_defs: BTreeSet<Name>,
  local_name: &Name,
  nam: &Name,
  nxt: &mut Box<Term>,
  mut rules: Vec<Rule>,
) -> (Term, BTreeSet<Name>, Vec<Rule>) {
  let mut fvs = BTreeSet::<Name>::new();
  for rule in rules.iter() {
    fvs.extend(rule.body.free_vars().into_keys().collect::<BTreeSet<_>>());
  }
  fvs.retain(|fv| !inner_defs.contains(fv));
  for rule in rules.iter() {
    for bind in rule.binds().flatten() {
      fvs.remove(bind);
    }
  }
  fvs.remove(nam);

  let call = Term::call(
    Term::Ref { nam: local_name.clone() },
    fvs.iter().cloned().map(|nam| Term::Var { nam }).collect::<Vec<_>>(),
  );

  for rule in rules.iter_mut() {
    let slf = std::mem::take(&mut rule.body);
    rule.body = Term::Use { nam: Some(nam.clone()), val: Box::new(call.clone()), nxt: Box::new(slf) };
  }

  let r#use = Term::Use { nam: Some(nam.clone()), val: Box::new(call.clone()), nxt: std::mem::take(nxt) };

  (r#use, fvs, rules)
}

fn apply_closure(rules: &mut [Rule], fvs: &BTreeSet<Name>) {
  for rule in rules.iter_mut() {
    let captured = fvs.iter().cloned().map(Some).collect::<Vec<_>>();
    rule.body = Term::rfold_lams(std::mem::take(&mut rule.body), captured.into_iter());
  }
}
