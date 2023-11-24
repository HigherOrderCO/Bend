use std::collections::{BTreeMap, HashSet};

use crate::term::{Book, DefId, DefNames, Definition, LetPat, MatchNum, Name, Rule, RulePat, Term};

/// Replaces closed Terms (i.e. without free variables) with a Ref to the extracted term
/// Precondition: Vars must have been sanitized
impl Book {
  pub fn detach_supercombinators(&mut self) {
    let mut combinators = Combinators::new();

    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.detach_combinators(def.def_id, &mut self.def_names, &mut combinators);
      }
    }

    // Definitions are not inserted to the book as they are defined to appease the borrow checker.
    // Since we are mut borrowing the rules we can't borrow the book to insert at the same time.
    self.defs.append(&mut combinators)
  }
}

type Combinators = BTreeMap<DefId, Definition>;

struct TermInfo<'d> {
  //Number of times a Term has been detached from the current Term
  counter: u32,
  rule_id: DefId,
  def_names: &'d mut DefNames,
  needed_names: HashSet<Name>,
  combinators: &'d mut Combinators,
}

impl<'d> TermInfo<'d> {
  fn new(rule_id: DefId, def_names: &'d mut DefNames, combinators: &'d mut Combinators) -> Self {
    Self { counter: 0, rule_id, def_names, needed_names: HashSet::new(), combinators }
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
    let name = self.def_names.name(&self.rule_id).unwrap();
    let comb_name = Name(format!("{name}${}", self.counter));
    self.counter += 1;

    let comb_id = self.def_names.insert(comb_name);

    let comb_var = Term::Ref { def_id: comb_id };
    let extracted_term = std::mem::replace(term, comb_var);

    let rules = vec![Rule { body: extracted_term, pats: Vec::new() }];
    let rule = Definition { def_id: comb_id, rules };
    self.combinators.insert(comb_id, rule);
  }
}

impl Term {
  pub fn detach_combinators(
    &mut self,
    rule_id: DefId,
    def_names: &mut DefNames,
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
        Term::Let { pat: LetPat::Var(nam), val, nxt } => {
          let val_is_super = go(val, depth + 1, term_info);
          let nxt_is_super = go(nxt, depth + 1, term_info);
          term_info.provide(nam);

          val_is_super && nxt_is_super
        }
        Term::Let { pat: LetPat::Tup(l_nam, r_nam), val, nxt } => {
          let val_is_super = go(val, depth + 1, term_info);
          let nxt_is_supper = go(nxt, depth + 1, term_info);

          if let Some(l_nam) = l_nam {
            term_info.provide(l_nam);
          }
          if let Some(r_nam) = r_nam {
            term_info.provide(r_nam);
          }

          val_is_super && nxt_is_supper
        }
        Term::Ref { .. } => true,
        Term::App { fun, arg } => {
          let fun_is_super = go(fun, depth + 1, term_info);
          let arg_is_super = go(arg, depth + 1, term_info);

          fun_is_super && arg_is_super
        }
        Term::Match { scrutinee, arms } => {
          let mut is_super = go(scrutinee, depth + 1, term_info);

          for (pat, term) in arms {
            let RulePat::Num(num) = pat else { unreachable!() };

            if let MatchNum::Succ(Some(nam)) = num {
              term_info.provide(nam);
            }

            is_super &= go(term, depth + 1, term_info);
          }

          is_super
        }
        Term::Dup { fst, snd, val, nxt, .. } => {
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
        Term::Num { .. } => true,
        Term::Opx { fst, snd, .. } => {
          let fst_is_super = go(fst, depth + 1, term_info);
          let snd_is_super = go(snd, depth + 1, term_info);

          fst_is_super && snd_is_super
        }
        Term::Tup { fst, snd } => {
          let fst_is_super = go(fst, depth + 1, term_info);
          let snd_is_super = go(snd, depth + 1, term_info);

          fst_is_super && snd_is_super
        }
      }
    }

    go(self, 0, &mut TermInfo::new(rule_id, def_names, combinators));
  }
}
