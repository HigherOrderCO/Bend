use hvm_core::Val;

use crate::ast::{hvm_lang::DefNames, var_id_to_name, DefId, Definition, DefinitionBook, Name, Rule, Term};
use std::collections::{HashMap, HashSet};

/// For all var declarations:
///   If they're used 0 times: erase the declaration
///   If they're used 1 time: leave them as-is
///   If they're used more times: insert dups to make var use affine
/// For all let vars:
///   If they're used 0 times: why? discard the let
///   If they're used 1 time: substitute the body in the var use
///   If they're use more times: add dups for all the uses, put the body at the root dup.
/// For all definition references: Convert from a Var term to an actual Ref term.
/// Reports any unbound variables.
/// Precondition: The pattern matching rules and constructors have already been converted into lambda calculus.

impl DefinitionBook {
  pub fn sanitize_vars(&mut self) -> anyhow::Result<()> {
    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        rule.body = rule.body.sanitize_vars(&self.def_names)?;
      }
    }
    Ok(())
  }
}

impl Term {
  pub fn sanitize_vars(&self, def_names: &DefNames) -> anyhow::Result<Term> {
    check_uses(self, def_names)?;
    let (body, mut var_uses) = unique_var_names(self, def_names)?;
    term_to_affine(body, &mut var_uses, &mut HashMap::new())
  }
}

/// Checks that all variables are bound and that all globals are used only once
fn check_uses(term: &Term, def_names: &DefNames) -> anyhow::Result<()> {
  fn push_scope<'a>(nam: &'a Name, scope: &mut HashMap<&'a Name, Val>) {
    if let Some(n_declarations) = scope.get_mut(nam) {
      *n_declarations += 1;
    } else {
      scope.insert(nam, 1);
    }
  }

  fn pop_scope(nam: &Name, scope: &mut HashMap<&Name, Val>) {
    let n_declarations = scope.get_mut(nam).unwrap();
    *n_declarations -= 1;
    if *n_declarations == 0 {
      scope.remove(nam);
    }
  }

  /// Scope has the number of times a name was declared in the current scope
  /// Globals has how many times a global var name was declared and used.
  fn go<'a>(
    term: &'a Term,
    scope: &mut HashMap<&'a Name, Val>,
    globals: &mut HashMap<&'a Name, (Val, Val)>,
    def_names: &DefNames,
  ) -> anyhow::Result<()> {
    // TODO: Don't stop at the first error
    match term {
      Term::Lam { nam: Some(nam), bod } => {
        push_scope(nam, scope);
        go(bod, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::Lam { nam: None, bod } => {
        go(bod, scope, globals, def_names)?;
      }
      Term::Var { nam } => {
        if !scope.contains_key(nam) && !def_names.contains_right(nam) {
          return Err(anyhow::anyhow!("Unbound variable '{nam}'"));
        }
      }
      Term::Chn { nam, bod } => {
        globals.entry(nam).or_default().0 += 1;
        go(bod, scope, globals, def_names)?;
      }
      Term::Lnk { nam } => {
        globals.entry(nam).or_default().1 += 1;
      }
      Term::Let { nam, val, nxt } => {
        go(val, scope, globals, def_names)?;
        push_scope(nam, scope);
        go(nxt, scope, globals, def_names)?;
        pop_scope(nam, scope);
      }
      Term::App { fun, arg } => {
        go(fun, scope, globals, def_names)?;
        go(arg, scope, globals, def_names)?;
      }
      Term::Dup { fst, snd, val, nxt } => {
        go(val, scope, globals, def_names)?;
        if let Some(fst) = fst {
          push_scope(fst, scope)
        }
        if let Some(snd) = snd {
          push_scope(snd, scope);
        }
        go(nxt, scope, globals, def_names)?;
        if let Some(snd) = snd {
          pop_scope(snd, scope);
        }
        if let Some(fst) = fst {
          pop_scope(fst, scope);
        }
      }
      Term::Sup { fst, snd } => {
        go(fst, scope, globals, def_names)?;
        go(snd, scope, globals, def_names)?;
      }
      Term::Ref { .. } | Term::Era => (),
      #[cfg(feature = "nums")]
      Term::Opx { fst, snd, .. } => {
        go(fst, scope, globals, def_names)?;
        go(snd, scope, globals, def_names)?;
      }
      #[cfg(feature = "nums")]
      Term::U32 { .. } | Term::I32 { .. } => (),
    }
    Ok(())
  }

  let mut globals = HashMap::new();
  go(term, &mut HashMap::new(), &mut globals, def_names)?;

  // Check global vars
  for (nam, (n_decls, n_uses)) in globals.into_iter() {
    if n_decls == 0 {
      return Err(anyhow::anyhow!("Global variable ${nam} declared more than once."));
    } else if n_uses == 0 {
      // TODO: Convert into local erased lambda
      return Err(anyhow::anyhow!("Global variable ${nam} was not used."));
    } else if n_uses > 1 {
      // TODO: Add dups for global vars
      return Err(anyhow::anyhow!("Global variable ${nam} used more than once."));
    }
  }
  Ok(())
}

type VarId = Val;
type UniqueNameScope = HashMap<Name, Vec<VarId>>;

/// Gives every declared variable a unique name and converts refs into Ref terms.
/// Also returns how many times each variable (with its new unique name) is used.
/// Precondition: No unbound variables in the term.
fn unique_var_names(term: &Term, def_names: &DefNames) -> anyhow::Result<(Term, HashMap<Name, Val>)> {
  fn push_name(
    name: Name,
    name_map: &mut UniqueNameScope,
    name_count: &mut VarId,
    var_uses: &mut HashMap<Name, Val>,
  ) {
    name_map.entry(name).or_default().push(*name_count);
    var_uses.insert(var_id_to_name(*name_count), 0);
    *name_count += 1;
  }

  fn pop_name(name: &Name, name_map: &mut UniqueNameScope) -> Name {
    let new_name = name_map.get_mut(name).unwrap().pop().unwrap();
    if name_map[name].is_empty() {
      name_map.remove(name);
    }
    var_id_to_name(new_name)
  }

  fn use_var(nam: &Name, name_map: &UniqueNameScope, var_uses: &mut HashMap<Name, Val>) -> Option<Name> {
    if let Some(vars) = name_map.get(nam) {
      let var_id = *vars.last().unwrap();
      let new_name = var_id_to_name(var_id);
      *var_uses.get_mut(&new_name).unwrap() += 1;
      Some(new_name)
    } else {
      None
    }
  }

  fn go(
    term: &Term,
    name_map: &mut UniqueNameScope,
    name_count: &mut VarId,
    var_uses: &mut HashMap<Name, Val>,
    def_names: &DefNames,
  ) -> anyhow::Result<Term> {
    let term = match term {
      Term::Lam { nam: Some(nam), bod } => {
        // Put the name in scope and assign it a unique id.
        // Convert the lambda body and then remove it from scope.
        // Return a lambda with the newly created name
        push_name(nam.clone(), name_map, name_count, var_uses);
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        let nam = pop_name(nam, name_map);
        Term::Lam { nam: Some(nam), bod: Box::new(bod) }
      }
      Term::Lam { nam: None, bod } => {
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Lam { nam: None, bod: Box::new(bod) }
      }
      Term::Var { nam } => {
        if let Some(nam) = use_var(nam, name_map, var_uses) {
          Term::Var { nam }
        } else {
          // If it's not in scope, we know it must be a Ref by our preconditions
          Term::Ref { def_id: *def_names.get_by_right(nam).unwrap() }
        }
      }
      Term::Chn { nam, bod } => {
        // Global lam names are already unique, so no need to do anything
        let bod = go(bod, name_map, name_count, var_uses, def_names)?;
        Term::Chn { nam: nam.clone(), bod: Box::new(bod) }
      }
      Term::Let { nam, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        push_name(nam.clone(), name_map, name_count, var_uses);
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let nam = pop_name(nam, name_map);
        Term::Let { nam, val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::App { fun, arg } => {
        let fun = go(fun, name_map, name_count, var_uses, def_names)?;
        let arg = go(arg, name_map, name_count, var_uses, def_names)?;
        Term::App { fun: Box::new(fun), arg: Box::new(arg) }
      }
      Term::Dup { fst, snd, val, nxt } => {
        let val = go(val, name_map, name_count, var_uses, def_names)?;
        if let Some(fst) = fst {
          push_name(fst.clone(), name_map, name_count, var_uses);
        }
        if let Some(snd) = snd {
          push_name(snd.clone(), name_map, name_count, var_uses);
        }
        let nxt = go(nxt, name_map, name_count, var_uses, def_names)?;
        let snd = snd.as_ref().map(|snd| pop_name(snd, name_map));
        let fst = fst.as_ref().map(|fst| pop_name(fst, name_map));
        Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
      }
      Term::Sup { fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Sup { fst: Box::new(fst), snd: Box::new(snd) }
      }
      #[cfg(feature = "nums")]
      Term::Opx { op, fst, snd } => {
        let fst = go(fst, name_map, name_count, var_uses, def_names)?;
        let snd = go(snd, name_map, name_count, var_uses, def_names)?;
        Term::Opx { op: *op, fst: Box::new(fst), snd: Box::new(snd) }
      }
      t => t.clone(),
    };
    Ok(term)
  }

  let mut var_uses = HashMap::new();
  let term = go(term, &mut Default::default(), &mut 0, &mut var_uses, def_names)?;
  Ok((term, var_uses))
}

/// Erases variables that weren't used, dups the ones that were used more than once.
/// Substitutes lets into their variable use.
/// Precondition: all variables in the term have unique names
fn term_to_affine(
  term: Term,
  var_uses: &mut HashMap<Name, Val>,
  let_bodies: &mut HashMap<Name, Term>,
) -> anyhow::Result<Term> {
  fn make_dup_tree(nam: &Name, mut nxt: Term, uses: Val, dup_body: Option<Term>) -> Term {
    // TODO: Is there a difference between a list of dups and a complete binary tree of dups?
    // Creates this: "dup x1 x1_dup = body; dup x2 x2_dup = x1_dup; dup x3 x4 = x2_dup; nxt"
    for i in (1 .. uses).rev() {
      nxt = Term::Dup {
        fst: Some(dup_name(nam, i)),
        snd: if i == uses - 1 { Some(dup_name(nam, uses)) } else { Some(internal_dup_name(nam, uses)) },
        val: if i == 1 {
          if let Some(dup_body) = &dup_body {
            Box::new(dup_body.clone()) // TODO: don't clone here
          } else {
            Box::new(Term::Var { nam: nam.clone() })
          }
        } else {
          Box::new(Term::Var { nam: internal_dup_name(nam, uses) })
        },
        nxt: Box::new(nxt),
      };
    }
    nxt
  }

  fn duplicate_lam(nam: Name, nxt: Term, uses: Val) -> (Term, Option<Name>) {
    // TODO: Is there a difference between a list of dups and a complete binary tree of dups?
    match uses {
      0 => (nxt, None),
      1 => (nxt, Some(dup_name(&nam, 1))),
      uses => (make_dup_tree(&nam, nxt, uses, None), Some(nam)),
    }
  }

  fn duplicate_let(nam: &Name, nxt: Term, uses: Val, let_body: Term) -> Term {
    make_dup_tree(nam, nxt, uses, Some(let_body))
  }

  fn dup_name(nam: &Name, uses: Val) -> Name {
    Name(format!("{nam}_{uses}"))
  }

  fn internal_dup_name(nam: &Name, uses: Val) -> Name {
    Name(format!("{}_dup", dup_name(nam, uses)))
  }

  let term = match term {
    Term::Lam { nam: None, bod } => {
      Term::Lam { nam: None, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) }
    }
    Term::Lam { nam: Some(nam), bod } => {
      if let Some(uses) = var_uses.get(&nam).copied() {
        let bod = term_to_affine(*bod, var_uses, let_bodies)?;
        let (bod, nam) = duplicate_lam(nam, bod, uses);
        Term::Lam { nam, bod: Box::new(bod) }
      } else {
        Term::Lam { nam: None, bod }
      }
    }
    Term::Var { nam } => {
      let uses = var_uses[&nam];
      *var_uses.get_mut(&nam).unwrap() -= 1;
      if let Some(subst) = let_bodies.remove(&nam) { subst } else { Term::Var { nam: dup_name(&nam, uses) } }
    }
    Term::Chn { nam, bod } => Term::Chn { nam, bod: Box::new(term_to_affine(*bod, var_uses, let_bodies)?) },
    Term::Let { nam, val, nxt } => {
      let uses = var_uses[&nam];
      match uses {
        0 => term_to_affine(*nxt, var_uses, let_bodies)?,
        1 => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let_bodies.insert(nam, val);
          term_to_affine(*nxt, var_uses, let_bodies)?
        }
        uses => {
          let val = term_to_affine(*val, var_uses, let_bodies)?;
          let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
          duplicate_let(&nam, nxt, uses, val)
        }
      }
    }
    Term::Dup { fst, snd, val, nxt } => {
      let uses_fst = fst.as_ref().map(|fst| *var_uses.get(fst).unwrap()).unwrap_or(0);
      let uses_snd = snd.as_ref().map(|snd| *var_uses.get(snd).unwrap()).unwrap_or(0);
      let val = term_to_affine(*val, var_uses, let_bodies)?;
      let nxt = term_to_affine(*nxt, var_uses, let_bodies)?;
      let (nxt, fst) = if let Some(fst) = fst { duplicate_lam(fst, nxt, uses_fst) } else { (nxt, fst) };
      let (nxt, snd) = if let Some(snd) = snd { duplicate_lam(snd, nxt, uses_snd) } else { (nxt, snd) };
      Term::Dup { fst, snd, val: Box::new(val), nxt: Box::new(nxt) }
    }
    Term::App { fun, arg } => Term::App {
      fun: Box::new(term_to_affine(*fun, var_uses, let_bodies)?),
      arg: Box::new(term_to_affine(*arg, var_uses, let_bodies)?),
    },
    Term::Sup { fst, snd } => Term::Sup {
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    #[cfg(feature = "nums")]
    Term::Opx { op, fst, snd } => Term::Opx {
      op,
      fst: Box::new(term_to_affine(*fst, var_uses, let_bodies)?),
      snd: Box::new(term_to_affine(*snd, var_uses, let_bodies)?),
    },
    t => t,
  };
  Ok(term)
}

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
    let Name(name) = self.def_names.get_by_left(&self.rule_id).unwrap();
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
