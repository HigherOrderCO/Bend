use crate::{
  maybe_grow,
  term::{Book, Name, Pattern, Term},
};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

/* Linearize preceding binds */

impl Book {
  /// Linearization of binds preceding match/switch terms, up to the
  /// first bind used in either the scrutinee or the bind.
  ///
  /// Example:
  /// ```hvm
  /// @a @b @c let d = (b c); switch a {
  ///   0: (A b c d)
  ///   _: (B a-1 b c d)
  /// }
  /// // Since `b`, `c` and `d` would be eta-reducible if linearized,
  /// // they get pushed inside the match.
  /// @a switch a {
  ///   0: @b @c let d = (b c); (A b c d)
  ///   _: @b @c let d = (b c); (B a-1 b c d)
  /// }
  /// ```
  pub fn linearize_match_binds(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_match_binds();
      }
    }
  }
}

impl Term {
  /// Linearize any binds preceding a match/switch term, up to the
  /// first bind used in either the scrutinee or the bind.
  pub fn linearize_match_binds(&mut self) {
    self.linearize_match_binds_go(vec![]);
  }

  fn linearize_match_binds_go(&mut self, mut bind_terms: Vec<Term>) {
    maybe_grow(|| match self {
      // Binding terms
      // Extract them in case they are preceding a match.
      Term::Lam { pat, bod, .. } if !pat.has_unscoped() => {
        let bod = std::mem::take(bod.as_mut());
        let term = std::mem::replace(self, bod);
        bind_terms.push(term);
        self.linearize_match_binds_go(bind_terms);
      }
      Term::Let { val, nxt, .. } | Term::Use { val, nxt, .. } => {
        val.linearize_match_binds_go(vec![]);
        if val.has_unscoped() {
          // Terms with unscoped can't be linearized since their names must be unique.
          nxt.linearize_match_binds_go(vec![]);
          self.wrap_with_bind_terms(bind_terms);
        } else {
          let nxt = std::mem::take(nxt.as_mut());
          let term = std::mem::replace(self, nxt);
          bind_terms.push(term);
          self.linearize_match_binds_go(bind_terms);
        }
      }

      // Matching terms
      Term::Mat { .. } | Term::Swt { .. } => {
        self.linearize_binds_single_match(bind_terms);
      }

      // Others
      // Not a match preceded by binds, so put the extracted terms back.
      term => {
        for child in term.children_mut() {
          child.linearize_match_binds_go(vec![]);
        }
        // Recover the extracted terms
        term.wrap_with_bind_terms(bind_terms);
      }
    })
  }

  fn linearize_binds_single_match(&mut self, bind_terms: Vec<Term>) {
    let (used_vars, with, arms) = match self {
      Term::Mat { arg, bnd: _, with, arms } => {
        let vars = arg.free_vars().into_keys().collect::<HashSet<_>>();
        let arms = arms.iter_mut().map(|arm| &mut arm.2).collect::<Vec<_>>();
        (vars, with, arms)
      }
      Term::Swt { arg, bnd: _, with, pred: _, arms } => {
        let vars = arg.free_vars().into_keys().collect::<HashSet<_>>();
        let arms = arms.iter_mut().collect();
        (vars, with, arms)
      }
      _ => unreachable!(),
    };

    let (non_linearized, linearized) = fixed_and_linearized_terms(used_vars, bind_terms);

    // Add the linearized terms to the arms and recurse
    for arm in arms {
      arm.wrap_with_bind_terms(linearized.clone());
      arm.linearize_match_binds_go(vec![]);
    }

    // Remove the linearized binds from the with clause
    let linearized_binds = linearized
      .iter()
      .flat_map(|t| match t {
        Term::Lam { pat, .. } | Term::Let { pat, .. } => pat.binds().flatten().collect::<Vec<_>>(),
        Term::Use { nam, .. } => {
          if let Some(nam) = nam {
            vec![nam]
          } else {
            vec![]
          }
        }
        _ => unreachable!(),
      })
      .collect::<HashSet<_>>();
    with.retain(|w| !linearized_binds.contains(w));

    // Add the non-linearized terms back to before the match
    self.wrap_with_bind_terms(non_linearized);
  }

  /// Given a term `self` and a sequence of `bind_terms`, wrap `self` with those binds.
  ///
  /// Example:
  /// ```hvm
  /// self = X
  /// match_terms = [λb *, let c = (a b); *, λd *]
  /// ```
  ///
  /// becomes
  ///
  /// ```hvm
  /// self = λb let c = (a b); λd X
  /// ```
  fn wrap_with_bind_terms(
    &mut self,
    bind_terms: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = Term>>,
  ) {
    *self = bind_terms.into_iter().rfold(std::mem::take(self), |acc, mut term| {
      match &mut term {
        Term::Lam { bod: nxt, .. } | Term::Let { nxt, .. } | Term::Use { nxt, .. } => {
          *nxt.as_mut() = acc;
        }
        _ => unreachable!(),
      }
      term
    });
  }
}

/// Separates the bind terms surround the match in two partitions,
/// one to be linearized, one to stay where they where.
///
/// We try to move down any binds that would become eta-reducible with linearization
/// and that will not introduce extra duplications.
///
/// This requires the bind to follow some rules:
/// * Can only depend on binds that will be moved
/// * Can't come before any bind that will not be moved.
/// * Must be a scoped bind.
///
/// Examples:
///
/// ```hvm
/// @a @b @c switch b { 0: c; _: (c b-1) }
/// // Will linearize `c` but not `a` since it comes before a lambda that can't be moved
/// // Becomes
/// @a @b switch b { 0: @c c; _: @c (c b-1) }
/// ```
///
/// ```hvm
/// @a let b = a; @c let e = b; let d = c; switch a { 0: X; _: Y }
/// // Will not linearize `let b = a` since it would duplicate `a`
/// // Will linearize `c` since it's a lambda that is not depended on by the argument
/// // Will not linearize `let e = b` since it would duplicate `b`
/// // Will linearize `let d = c` since it depends only on variables that will be moved
/// // and is not depended on by the argument
/// ```
fn fixed_and_linearized_terms(used_in_arg: HashSet<Name>, bind_terms: Vec<Term>) -> (Vec<Term>, Vec<Term>) {
  let fixed_binds = binds_fixed_by_dependency(used_in_arg, &bind_terms);

  let mut fixed = VecDeque::new();
  let mut linearized = VecDeque::new();
  let mut stop = false;
  for term in bind_terms.into_iter().rev() {
    let to_linearize = match &term {
      Term::Use { nam, .. } => nam.as_ref().map_or(true, |nam| !fixed_binds.contains(nam)),
      Term::Let { pat, .. } => pat.binds().flatten().all(|nam| !fixed_binds.contains(nam)),
      Term::Lam { pat, .. } => pat.binds().flatten().all(|nam| !fixed_binds.contains(nam)),
      _ => unreachable!(),
    };
    let to_linearize = to_linearize && !stop;
    if to_linearize {
      linearized.push_front(term);
    } else {
      if matches!(term, Term::Lam { .. }) {
        stop = true;
      }
      fixed.push_front(term);
    }
  }
  (fixed.into_iter().collect(), linearized.into_iter().collect())
}

/// Get which binds are fixed because they are in the dependency graph
/// of a free var or of a var used in the match arg.
fn binds_fixed_by_dependency(mut fixed_binds: HashSet<Name>, bind_terms: &[Term]) -> HashSet<Name> {
  // Find the use dependencies of each bind
  let mut binds = vec![];
  let mut dependency_digraph = HashMap::new();
  for term in bind_terms {
    // Gather what are the binds of this term and what vars it is directly using
    let (term_binds, term_uses) = match term {
      Term::Lam { pat, .. } => {
        let binds = pat.binds().flatten().cloned().collect::<Vec<_>>();
        (binds, vec![])
      }
      Term::Let { pat, val, .. } => {
        let binds = pat.binds().flatten().cloned().collect::<Vec<_>>();
        let uses = val.free_vars().into_keys().collect();
        (binds, uses)
      }
      Term::Use { nam, val, .. } => {
        let binds = if let Some(nam) = nam { vec![nam.clone()] } else { vec![] };
        let uses = val.free_vars().into_keys().collect();
        (binds, uses)
      }
      _ => unreachable!(),
    };

    for bind in term_binds {
      dependency_digraph.insert(bind.clone(), term_uses.clone());
      binds.push(bind);
    }
  }

  // Mark binds that depend on free vars as fixed
  for (bind, deps) in dependency_digraph.iter() {
    if deps.iter().any(|dep| !binds.contains(dep)) {
      fixed_binds.insert(bind.clone());
    }
  }

  // Convert to undirected graph
  let mut dependency_graph: HashMap<Name, HashSet<Name>> =
    HashMap::from_iter(binds.iter().map(|k| (k.clone(), HashSet::new())));
  for (bind, deps) in dependency_digraph {
    for dep in deps {
      if !binds.contains(&dep) {
        dependency_graph.insert(dep.clone(), HashSet::new());
      }
      dependency_graph.get_mut(&dep).unwrap().insert(bind.clone());
      dependency_graph.get_mut(&bind).unwrap().insert(dep);
    }
  }

  // Find which binds are connected to the vars used in the match arg or to free vars.
  let mut used_component = HashSet::new();
  let mut visited = HashSet::new();
  let mut to_visit = fixed_binds.iter().collect::<Vec<_>>();
  while let Some(node) = to_visit.pop() {
    if visited.contains(node) {
      continue;
    }
    used_component.insert(node.clone());
    visited.insert(node);

    // Add these dependencies to be checked (if it's not a free var in the match arg)
    if let Some(deps) = dependency_graph.get(node) {
      to_visit.extend(deps);
    }
  }
  used_component
}

/* Linearize all used vars */

impl Book {
  /// Linearizes all variables used in a matches' arms.
  pub fn linearize_matches(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_matches();
      }
    }
  }
}

impl Term {
  fn linearize_matches(&mut self) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.linearize_matches();
      }

      if matches!(self, Term::Mat { .. } | Term::Swt { .. }) {
        lift_match_vars(self);
      }
    })
  }
}

/// Converts free vars inside the match arms into lambdas with
/// applications around the match to pass them the external value.
///
/// Makes the rules extractable and linear (no need for dups even
/// when a variable is used in multiple rules).
///
/// Obs: This does not modify unscoped variables.
pub fn lift_match_vars(match_term: &mut Term) -> &mut Term {
  // Collect match arms with binds
  let arms: Vec<_> = match match_term {
    Term::Mat { arg: _, bnd: _, with: _, arms: rules } => {
      rules.iter().map(|(_, binds, body)| (binds.iter().flatten().cloned().collect(), body)).collect()
    }
    Term::Swt { arg: _, bnd: _, with: _, pred, arms } => {
      let (succ, nums) = arms.split_last_mut().unwrap();
      let mut arms = nums.iter().map(|body| (vec![], body)).collect::<Vec<_>>();
      arms.push((vec![pred.clone().unwrap()], succ));
      arms
    }
    _ => unreachable!(),
  };

  // Collect all free vars in the match arms
  let mut free_vars = Vec::<Vec<_>>::new();
  for (binds, body) in arms {
    let mut arm_free_vars = body.free_vars();
    for bind in binds {
      arm_free_vars.remove(&bind);
    }
    free_vars.push(arm_free_vars.into_keys().collect());
  }

  // Collect the vars to lift
  // We need consistent iteration order.
  let vars_to_lift: BTreeSet<Name> = free_vars.into_iter().flatten().collect();

  // Add lambdas to the arms
  match match_term {
    Term::Mat { arg: _, bnd: _, with, arms } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for arm in arms {
        let old_body = std::mem::take(&mut arm.2);
        arm.2 =
          vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::lam(Pattern::Var(Some(nam)), body));
      }
    }
    Term::Swt { arg: _, bnd: _, with, pred: _, arms } => {
      with.retain(|with| !vars_to_lift.contains(with));
      for arm in arms {
        let old_body = std::mem::take(arm);
        *arm =
          vars_to_lift.iter().cloned().rfold(old_body, |body, nam| Term::lam(Pattern::Var(Some(nam)), body));
      }
    }
    _ => unreachable!(),
  }

  // Add apps to the match
  *match_term = vars_to_lift.into_iter().fold(std::mem::take(match_term), Term::arg_call);

  get_match_reference(match_term)
}

/// Get a reference to the match again
/// It returns a reference and not an owned value because we want
/// to keep the new surrounding Apps but still modify the match further.
fn get_match_reference(mut match_term: &mut Term) -> &mut Term {
  loop {
    match match_term {
      Term::App { tag: _, fun, arg: _ } => match_term = fun.as_mut(),
      Term::Swt { .. } | Term::Mat { .. } => return match_term,
      _ => unreachable!(),
    }
  }
}

/* Linearize `with` vars  */

impl Book {
  /// Linearizes all variables specified in the `with` clauses of match terms.
  pub fn linearize_match_with(&mut self) {
    for def in self.defs.values_mut() {
      for rule in def.rules.iter_mut() {
        rule.body.linearize_match_with();
      }
    }
  }
}

impl Term {
  fn linearize_match_with(&mut self) {
    maybe_grow(|| {
      for child in self.children_mut() {
        child.linearize_match_with();
      }
    });
    match self {
      Term::Mat { arg: _, bnd: _, with, arms: rules } => {
        // Linearize the vars in the `with` clause, but only the used ones.
        let with = retain_used_names(std::mem::take(with), rules.iter().map(|r| &r.2));
        for rule in rules {
          rule.2 = with
            .iter()
            .rfold(std::mem::take(&mut rule.2), |bod, nam| Term::lam(Pattern::Var(Some(nam.clone())), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      Term::Swt { arg: _, bnd: _, with, pred: _, arms } => {
        let with = retain_used_names(std::mem::take(with), arms.iter());
        for arm in arms {
          *arm = with
            .iter()
            .rfold(std::mem::take(arm), |bod, nam| Term::lam(Pattern::Var(Some(nam.clone())), bod));
        }
        *self = Term::call(std::mem::take(self), with.into_iter().map(|nam| Term::Var { nam }));
      }
      _ => {}
    }
  }
}

/// From a Vec of variable names, return the ones that are used inside `terms`.
fn retain_used_names<'a>(mut names: Vec<Name>, terms: impl IntoIterator<Item = &'a Term>) -> Vec<Name> {
  let mut used_names = HashSet::new();
  for term in terms.into_iter() {
    let mut free_vars = term.free_vars();
    free_vars.retain(|_, uses| *uses > 0);
    used_names.extend(free_vars.into_keys());
  }
  names.retain(|nam| used_names.contains(nam));
  names
}
