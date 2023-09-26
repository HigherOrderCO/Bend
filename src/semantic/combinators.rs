use crate::ast::{hvm_lang::DefNames, DefId, Definition, DefinitionBook, Name, Rule, Term};
use std::collections::HashSet;

/// Replaces closed Terms (i.e. without free variables) with a Ref to the extracted term
/// Precondition: Vars must have been sanitized
impl DefinitionBook {
  pub fn detach_combinators(&mut self) {
    let mut combinators = Vec::new();
    let new_ids_base = self.defs.len() as u32;

    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.detach_combinators(rule.def_id, new_ids_base, &self.def_names, &mut combinators);
      }
    }

    for (name, def_id, body) in combinators {
      self.def_names.insert(def_id, name);
      let rules = vec![Rule { def_id, pats: Vec::new(), body }];

      self.defs.push(Definition { def_id, rules })
    }
  }
}

type Combinators = Vec<(Name, DefId, Term)>;

struct TermInfo<'d> {
  //Number of times a Term has been detached from the current Term
  counter: u32,
  //Base value for creating new DefIds
  new_ids_base: u32,
  rule_id: DefId,
  def_names: &'d DefNames,
  needed_names: HashSet<Name>,
  combinators: &'d mut Combinators,
}

impl<'d> TermInfo<'d> {
  fn new(
    new_ids_base: u32,
    rule_id: DefId,
    def_names: &'d DefNames,
    combinators: &'d mut Combinators,
  ) -> Self {
    Self { counter: 0, new_ids_base, rule_id, def_names, needed_names: HashSet::new(), combinators }
  }
  fn request_name(&mut self, name: &Name) {
    self.needed_names.insert(name.to_owned());
  }

  fn provide(&mut self, name: &Name) {
    self.needed_names.remove(name);
  }

  fn check(&self) -> bool {
    self.needed_names.is_empty()
  }

  fn replace_scope(&mut self, new_scope: HashSet<Name>) -> HashSet<Name> {
    std::mem::replace(&mut self.needed_names, new_scope)
  }

  fn merge_scope(&mut self, target: HashSet<Name>) {
    self.needed_names.extend(target);
  }

  fn detach_term(&mut self, term: &mut Term) {
    let Name(name) = self.def_names.name(&self.rule_id).unwrap();
    let combinator = Name(format!("{name}{}", self.counter));
    self.counter += 1;

    let def_id = DefId(self.new_ids_base + (self.combinators.len() as u32));

    let combinator_var = Term::Ref { def_id };
    let extracted_term = std::mem::replace(term, combinator_var);

    self.combinators.push((combinator, def_id, extracted_term));
  }
}

impl Term {
  pub fn detach_combinators(
    &mut self,
    rule_id: DefId,
    new_ids_base: u32,
    def_names: &DefNames,
    combinators: &mut Combinators,
  ) {
    fn go(term: &mut Term, depth: usize, term_info: &mut TermInfo) -> bool {
      match term {
        Term::Lam { nam, bod } => {
          let parent_scope = term_info.replace_scope(HashSet::new());

          let is_super = go(bod, depth + 1, term_info);

          if let Some(name) = nam {
            term_info.provide(name);
          }

          if is_super && depth != 0 && term_info.check() {
            term_info.detach_term(term);
          }

          term_info.merge_scope(parent_scope);

          is_super
        }
        Term::Var { nam } => {
          term_info.request_name(nam);
          true
        }
        Term::Chn { nam: _, bod } => {
          go(bod, depth + 1, term_info);
          false
        }
        Term::Lnk { .. } => false,
        Term::Let { nam, val, nxt } => {
          let val_is_super = go(val, depth + 1, term_info);
          let nxt_is_super = go(nxt, depth + 1, term_info);
          term_info.provide(nam);

          val_is_super && nxt_is_super
        }
        Term::Ref { .. } => true,
        Term::App { fun, arg } => {
          let fun_is_super = go(fun, depth + 1, term_info);
          let arg_is_super = go(arg, depth + 1, term_info);

          fun_is_super && arg_is_super
        }
        Term::Dup { fst, snd, val, nxt } => {
          let val_is_super = go(val, depth + 1, term_info);
          let nxt_is_supper = go(nxt, depth + 1, term_info);

          if let Some(snd) = snd {
            term_info.provide(snd);
          }
          if let Some(fst) = fst {
            term_info.provide(fst);
          }

          val_is_super && nxt_is_supper
        }

        Term::Sup { .. } => todo!(),
        Term::Era => true,

        #[cfg(feature = "nums")]
        Term::U32 { .. } => true,
        #[cfg(feature = "nums")]
        Term::I32 { .. } => true,
        #[cfg(feature = "nums")]
        Term::Opx { .. } => true,
      }
    }

    go(self, 0, &mut TermInfo::new(new_ids_base, rule_id, def_names, combinators));
  }
}
