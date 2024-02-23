use crate::term::{Book, Definition, MatchNum, Name, Pattern, Rule, Term};
use std::{
  collections::{BTreeMap, BTreeSet},
  ops::BitAnd,
};

/// Replaces closed Terms (i.e. without free variables) with a Ref to the extracted term
/// Precondition: Vars must have been sanitized
impl Book {
  pub fn detach_supercombinators(&mut self) {
    let mut combinators = Combinators::new();

    for (def_name, def) in self.defs.iter_mut() {
      if self.entrypoint.as_ref().is_some_and(|m| m == def_name) {
        continue;
      }

      let builtin = def.builtin;
      let rule = def.rule_mut();
      rule.body.detach_combinators(def_name, builtin, &mut combinators);
    }

    // Definitions are not inserted to the book as they are defined to appease the borrow checker.
    // Since we are mut borrowing the rules we can't borrow the book to insert at the same time.
    self.defs.extend(combinators);
  }
}

type Combinators = BTreeMap<Name, Definition>;

#[derive(Debug)]
struct TermInfo<'d> {
  // Number of times a Term has been detached from the current Term
  counter: u32,
  def_name: Name,
  builtin: bool,
  needed_names: BTreeSet<Name>,
  combinators: &'d mut Combinators,
}

impl<'d> TermInfo<'d> {
  fn new(def_name: Name, builtin: bool, combinators: &'d mut Combinators) -> Self {
    Self { counter: 0, def_name, builtin, needed_names: BTreeSet::new(), combinators }
  }
  fn request_name(&mut self, name: &Name) {
    self.needed_names.insert(name.clone());
  }

  fn provide(&mut self, name: Option<&Name>) {
    if let Some(name) = name {
      self.needed_names.remove(name);
    }
  }

  fn has_no_free_vars(&self) -> bool {
    self.needed_names.is_empty()
  }

  fn replace_scope(&mut self, new_scope: BTreeSet<Name>) -> BTreeSet<Name> {
    std::mem::replace(&mut self.needed_names, new_scope)
  }

  fn merge_scope(&mut self, target: BTreeSet<Name>) {
    self.needed_names.extend(target);
  }

  fn detach_term(&mut self, term: &mut Term) {
    let comb_name = Name::new(format!("{}$S{}", self.def_name, self.counter));
    self.counter += 1;

    let comb_var = Term::Ref { nam: comb_name.clone() };
    let extracted_term = std::mem::replace(term, comb_var);

    let rules = vec![Rule { body: extracted_term, pats: Vec::new() }];
    let rule = Definition { name: comb_name.clone(), rules, builtin: self.builtin };
    self.combinators.insert(comb_name, rule);
  }
}

#[derive(Debug)]
enum Detach {
  /// Can be detached freely
  Combinator,
  /// Can not be detached
  Unscoped { lams: BTreeSet<Name>, vars: BTreeSet<Name> },
  /// Should be detached to make the program not hang
  Recursive,
}

impl Detach {
  fn can_detach(&self) -> bool {
    !matches!(self, Detach::Unscoped { .. })
  }

  fn unscoped_lam(nam: Name) -> Self {
    Detach::Unscoped { lams: [nam].into(), vars: Default::default() }
  }

  fn unscoped_var(nam: Name) -> Self {
    Detach::Unscoped { lams: Default::default(), vars: [nam].into() }
  }
}

impl BitAnd for Detach {
  type Output = Detach;

  fn bitand(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Detach::Combinator, Detach::Combinator) => Detach::Combinator,

      (Detach::Combinator, unscoped @ Detach::Unscoped { .. }) => unscoped,
      (unscoped @ Detach::Unscoped { .. }, Detach::Combinator) => unscoped,

      // Merge two unscoped terms into one, removing entries of each matching `Lam` and `Var`.
      // If all unscoped pairs are found and removed this way, the term can be treated as a Combinator.
      (Detach::Unscoped { mut lams, mut vars }, Detach::Unscoped { lams: rhs_lams, vars: rhs_vars }) => {
        lams.extend(rhs_lams);
        vars.extend(rhs_vars);

        let res_lams: BTreeSet<_> = lams.difference(&vars).cloned().collect();
        let res_vars: BTreeSet<_> = vars.difference(&lams).cloned().collect();

        if res_lams.is_empty() && res_vars.is_empty() {
          Detach::Combinator
        } else {
          Detach::Unscoped { lams: res_lams, vars: res_vars }
        }
      }

      (Detach::Combinator, Detach::Recursive) => Detach::Recursive,
      (Detach::Recursive, Detach::Combinator) => Detach::Recursive,
      (Detach::Recursive, Detach::Recursive) => Detach::Recursive,

      // TODO: Is this the best way to best deal with a term that is both unscoped and recursive?
      (unscoped @ Detach::Unscoped { .. }, Detach::Recursive) => unscoped,
      (Detach::Recursive, unscoped @ Detach::Unscoped { .. }) => unscoped,
    }
  }
}

impl Term {
  pub fn detach_combinators(&mut self, def_name: &Name, builtin: bool, combinators: &mut Combinators) {
    fn go_lam(term: &mut Term, depth: usize, term_info: &mut TermInfo) -> Detach {
      let parent_scope = term_info.replace_scope(BTreeSet::new());

      let (nam, bod, unscoped): (Option<&Name>, &mut Term, bool) = match term {
        Term::Lam { nam, bod, .. } => (nam.as_ref(), bod, false),
        Term::Chn { nam, bod, .. } => (Some(nam), bod, true),
        _ => unreachable!(),
      };

      let mut detach = go(bod, depth + 1, term_info);

      if unscoped {
        detach = detach & Detach::unscoped_lam(nam.cloned().unwrap());
      }

      if !unscoped {
        term_info.provide(nam);
      }

      if detach.can_detach() && !term.is_id() && depth != 0 && term_info.has_no_free_vars() {
        term_info.detach_term(term);
      }

      term_info.merge_scope(parent_scope);

      detach
    }

    fn go(term: &mut Term, depth: usize, term_info: &mut TermInfo) -> Detach {
      match term {
        Term::Lam { .. } => go_lam(term, depth, term_info),
        Term::Chn { .. } => go_lam(term, depth, term_info),

        Term::App { fun, arg, .. } => {
          let parent_scope = term_info.replace_scope(BTreeSet::new());

          let fun_detach = go(fun, depth + 1, term_info);
          let fun_scope = term_info.replace_scope(BTreeSet::new());

          let arg_detach = go(arg, depth + 1, term_info);
          let arg_scope = term_info.replace_scope(parent_scope);

          let detach = match fun_detach {
            // If the fun scope is not empty, we know the recursive ref is not in a active position,
            // Because if it was an application, it would be already extracted
            //
            // e.g.: {recursive_ref some_var}
            Detach::Recursive if !fun_scope.is_empty() => arg_detach,

            Detach::Recursive if arg_scope.is_empty() && arg_detach.can_detach() => {
              term_info.detach_term(term);
              Detach::Combinator
            }

            // If the only term that is possible to detach is just a Term::Ref,
            // there is no benefit to it, so that case is skipped
            Detach::Recursive if !matches!(fun, box Term::Ref { .. }) => {
              term_info.detach_term(fun);
              arg_detach
            }

            _ => fun_detach & arg_detach,
          };

          term_info.merge_scope(fun_scope);
          term_info.merge_scope(arg_scope);

          detach
        }

        Term::Var { nam } => {
          term_info.request_name(nam);
          Detach::Combinator
        }

        Term::Lnk { nam } => Detach::unscoped_var(nam.clone()),

        Term::Let { pat: Pattern::Var(nam), val, nxt } => {
          let val_detach = go(val, depth + 1, term_info);
          let nxt_detach = go(nxt, depth + 1, term_info);
          term_info.provide(nam.as_ref());

          val_detach & nxt_detach
        }
        Term::Dup { fst, snd, val, nxt, .. }
        | Term::Let { pat: Pattern::Tup(box Pattern::Var(fst), box Pattern::Var(snd)), val, nxt } => {
          let val_detach = go(val, depth + 1, term_info);
          let nxt_detach = go(nxt, depth + 1, term_info);

          term_info.provide(fst.as_ref());
          term_info.provide(snd.as_ref());

          val_detach & nxt_detach
        }
        Term::Mat { args, rules } => {
          let mut detach = Detach::Combinator;
          for arg in args {
            detach = detach & go(arg, depth + 1, term_info);
          }
          let parent_scope = term_info.replace_scope(BTreeSet::new());

          for rule in rules.iter_mut() {
            for pat in &rule.pats {
              debug_assert!(pat.is_detached_num_match());
            }

            let arm_detach = match go(&mut rule.body, depth + 1, term_info) {
              // If the recursive ref reached here, it is not in a active position
              Detach::Recursive => Detach::Combinator,
              detach => detach,
            };

            // It is expected that match arms were already linearized
            detach = detach & arm_detach;
          }

          // This happens when a var is used in only one arm so it is not linearized
          if !term_info.has_no_free_vars() {
            for rule in rules {
              let (arm_body, already_extracted) = match rule.pats[0] {
                Pattern::Num(MatchNum::Zero) => (&mut rule.body, false),
                _ => match &mut rule.body {
                  Term::Lam { bod, .. } => (bod.as_mut(), false),
                  Term::Ref { nam } => {
                    let extracted = term_info.combinators.get_mut(nam).unwrap();
                    let Term::Lam { ref mut bod, .. } = &mut extracted.rules[0].body else { unreachable!() };
                    (bod.as_mut(), true)
                  }
                  _ => unreachable!(),
                },
              };

              let term = std::mem::take(arm_body);

              let body_vars = term.free_vars();

              let term = term_info.needed_names.iter().rev().fold(term, |acc, arg| {
                if body_vars.contains_key(arg) {
                  Term::named_lam(arg.clone(), acc)
                } else {
                  Term::lam(None, acc)
                }
              });

              *arm_body = term;
              if !already_extracted {
                term_info.detach_term(&mut rule.body);
              }
            }

            let mat = std::mem::take(term);
            *term = term_info
              .needed_names
              .iter()
              .fold(mat, |acc, arg| Term::app(acc, Term::Var { nam: arg.clone() }));
          }

          term_info.merge_scope(parent_scope);
          detach
        }
        Term::Sup { fst, snd, .. } | Term::Tup { fst, snd } | Term::Opx { fst, snd, .. } => {
          let fst_is_super = go(fst, depth + 1, term_info);
          let snd_is_super = go(snd, depth + 1, term_info);

          fst_is_super & snd_is_super
        }
        Term::Ref { nam: def_name } if def_name == &term_info.def_name => Detach::Recursive,
        Term::Let { .. } | Term::Lst { .. } => unreachable!(),
        Term::Ref { .. } | Term::Num { .. } | Term::Str { .. } | Term::Era | Term::Err => Detach::Combinator,
      }
    }

    go(self, 0, &mut TermInfo::new(def_name.clone(), builtin, combinators));
  }

  // We don't want to detach id function, since that's not a net gain in performance or space
  fn is_id(&self) -> bool {
    match self {
      Term::Lam { nam: Some(lam_nam), bod: box Term::Var { nam: var_nam }, .. } => lam_nam == var_nam,
      _ => false,
    }
  }
}
