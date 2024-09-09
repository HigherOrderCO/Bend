//! Optional Hindley-Milner-like type system.
//!
//! Based on https://github.com/developedby/algorithm-w-rs
//! and https://github.com/mgrabmueller/AlgorithmW.
use crate::{
  diagnostics::Diagnostics,
  fun::{num_to_name, Adt, Book, Ctx, FanKind, MatchRule, Name, Num, Op, Pattern, Tag, Term, Type},
  maybe_grow,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};

impl Ctx<'_> {
  pub fn type_check(&mut self) -> Result<(), Diagnostics> {
    let types = infer_book(self.book, &mut self.info)?;

    for def in self.book.defs.values_mut() {
      def.typ = types[&def.name].instantiate(&mut VarGen::default());
    }

    Ok(())
  }
}

type ProgramTypes = HashMap<Name, Scheme>;

/// A type scheme, aka a polymorphic type.
#[derive(Clone)]
struct Scheme(Vec<Name>, Type);

/// A finite mapping from type variables to types.
#[derive(Clone, Default)]
struct Subst(BTreeMap<Name, Type>);

/// A mapping from term variables to type schemes.
#[derive(Clone, Default)]
struct TypeEnv(BTreeMap<Name, Scheme>);

/// Variable generator for type variables.
#[derive(Default)]
struct VarGen(usize);

/// Topologically ordered set of mutually recursive groups of functions.
struct RecGroups(Vec<Vec<Name>>);

/* Implementations */

impl Type {
  fn free_type_vars(&self) -> BTreeSet<Name> {
    maybe_grow(|| match self {
      Type::Var(x) => BTreeSet::from([x.clone()]),
      Type::Ctr(_, ts) | Type::Tup(ts) => ts.iter().flat_map(|t| t.free_type_vars()).collect(),
      Type::Arr(t1, t2) => t1.free_type_vars().union(&t2.free_type_vars()).cloned().collect(),
      Type::Number(t) | Type::Integer(t) => t.free_type_vars(),
      Type::U24 | Type::F24 | Type::I24 | Type::None | Type::Any | Type::Hole => BTreeSet::new(),
    })
  }

  fn subst(&self, subst: &Subst) -> Type {
    maybe_grow(|| match self {
      Type::Var(nam) => match subst.0.get(nam) {
        Some(new) => new.clone(),
        None => self.clone(),
      },
      Type::Ctr(name, ts) => Type::Ctr(name.clone(), ts.iter().map(|t| t.subst(subst)).collect()),
      Type::Arr(t1, t2) => Type::Arr(Box::new(t1.subst(subst)), Box::new(t2.subst(subst))),
      Type::Tup(els) => Type::Tup(els.iter().map(|t| t.subst(subst)).collect()),
      Type::Number(t) => Type::Number(Box::new(t.subst(subst))),
      Type::Integer(t) => Type::Integer(Box::new(t.subst(subst))),
      t @ (Type::U24 | Type::F24 | Type::I24 | Type::None | Type::Any | Type::Hole) => t.clone(),
    })
  }

  /// Converts a monomorphic type into a type scheme by abstracting
  /// over the type variables free in `t`, but not free in the type
  /// environment.
  fn generalize(&self, env: &TypeEnv) -> Scheme {
    let vars_env = env.free_type_vars();
    let vars_t = self.free_type_vars();
    let vars = vars_t.difference(&vars_env).cloned().collect();
    Scheme(vars, self.clone())
  }
}

impl Scheme {
  fn free_type_vars(&self) -> BTreeSet<Name> {
    let vars = self.1.free_type_vars();
    let bound_vars = self.0.iter().cloned().collect();
    vars.difference(&bound_vars).cloned().collect()
  }

  fn subst(&self, subst: &Subst) -> Scheme {
    let mut subst = subst.clone();
    for x in self.0.iter() {
      subst.0.remove(x);
    }
    let t = self.1.subst(&subst);
    Scheme(self.0.clone(), t)
  }

  /// Converts a type scheme into a monomorphic type by assigning
  /// fresh type variables to each variable bound by the scheme.
  fn instantiate(&self, var_gen: &mut VarGen) -> Type {
    let new_vars = self.0.iter().map(|_| var_gen.fresh());
    let subst = Subst(self.0.iter().cloned().zip(new_vars).collect());
    self.1.subst(&subst)
  }
}

impl Subst {
  /// Compose two substitutions.
  ///
  /// Applies the first substitution to the second, and then inserts the result into the first.
  fn compose(mut self, other: Subst) -> Subst {
    let other = other.0.into_iter().map(|(x, t)| (x, t.subst(&self))).collect::<Vec<_>>();
    self.0.extend(other);
    self
  }
}

impl TypeEnv {
  fn free_type_vars(&self) -> BTreeSet<Name> {
    let mut vars = BTreeSet::new();
    for scheme in self.0.values() {
      let scheme_vars = scheme.free_type_vars();
      vars = vars.union(&scheme_vars).cloned().collect();
    }
    vars
  }

  fn subst(&self, subst: &Subst) -> TypeEnv {
    let env = self.0.iter().map(|(x, scheme)| (x.clone(), scheme.subst(subst))).collect();
    TypeEnv(env)
  }

  fn insert(&mut self, name: Name, scheme: Scheme) {
    self.0.insert(name, scheme);
  }

  fn add_binds<'a>(
    &mut self,
    bnd: impl IntoIterator<Item = (&'a Option<Name>, Scheme)>,
  ) -> Vec<(Name, Option<Scheme>)> {
    let mut old_bnd = vec![];
    for (name, scheme) in bnd {
      if let Some(name) = name {
        let old = self.0.insert(name.clone(), scheme);
        old_bnd.push((name.clone(), old));
      }
    }
    old_bnd
  }

  fn pop_binds(&mut self, old_bnd: Vec<(Name, Option<Scheme>)>) {
    for (name, scheme) in old_bnd {
      if let Some(scheme) = scheme {
        self.0.insert(name, scheme);
      }
    }
  }
}

impl VarGen {
  fn fresh(&mut self) -> Type {
    let x = self.fresh_name();
    Type::Var(x)
  }

  fn fresh_name(&mut self) -> Name {
    let x = num_to_name(self.0 as u64);
    self.0 += 1;
    Name::new(x)
  }
}

impl RecGroups {
  fn from_book(book: &Book) -> RecGroups {
    type DependencyGraph<'a> = BTreeMap<&'a Name, BTreeSet<&'a Name>>;

    fn collect_dependencies<'a>(
      term: &'a Term,
      book: &'a Book,
      scope: &mut Vec<Name>,
      deps: &mut BTreeSet<&'a Name>,
    ) {
      if let Term::Ref { nam } = term {
        if book.ctrs.contains_key(nam) || book.hvm_defs.contains_key(nam) || !book.defs[nam].check {
          // Don't infer types for constructors or unchecked functions
        } else {
          deps.insert(nam);
        }
      }
      for (child, binds) in term.children_with_binds() {
        scope.extend(binds.clone().flatten().cloned());
        collect_dependencies(child, book, scope, deps);
        scope.truncate(scope.len() - binds.flatten().count());
      }
    }

    /// Tarjan's algorithm for finding strongly connected components.
    fn strong_connect<'a>(
      v: &'a Name,
      deps: &DependencyGraph<'a>,
      index: &mut usize,
      index_map: &mut BTreeMap<&'a Name, usize>,
      low_link: &mut BTreeMap<&'a Name, usize>,
      stack: &mut Vec<&'a Name>,
      components: &mut Vec<BTreeSet<Name>>,
    ) {
      maybe_grow(|| {
        index_map.insert(v, *index);
        low_link.insert(v, *index);
        *index += 1;
        stack.push(v);

        if let Some(neighbors) = deps.get(v) {
          for w in neighbors {
            if !index_map.contains_key(w) {
              // Successor w has not yet been visited, recurse on it.
              strong_connect(w, deps, index, index_map, low_link, stack, components);
              low_link.insert(v, low_link[v].min(low_link[w]));
            } else if stack.contains(w) {
              // Successor w is in stack S and hence in the current SCC.
              low_link.insert(v, low_link[v].min(index_map[w]));
            } else {
              // If w is not on stack, then (v, w) is an edge pointing
              // to an SCC already found and must be ignored.
            }
          }
        }

        // If v is a root node, pop the stack and generate an SCC.
        if low_link[v] == index_map[v] {
          let mut component = BTreeSet::new();
          while let Some(w) = stack.pop() {
            component.insert(w.clone());
            if w == v {
              break;
            }
          }
          components.push(component);
        }
      })
    }

    // Build the dependency graph
    let mut deps = DependencyGraph::default();
    for (name, def) in &book.defs {
      if book.ctrs.contains_key(name) || !def.check {
        // Don't infer types for constructors or unchecked functions
        continue;
      }
      let mut fn_deps = Default::default();
      collect_dependencies(&def.rule().body, book, &mut vec![], &mut fn_deps);
      deps.insert(name, fn_deps);
    }

    let mut index = 0;
    let mut stack = Vec::new();
    let mut index_map = BTreeMap::new();
    let mut low_link = BTreeMap::new();
    let mut components = Vec::new();
    for name in deps.keys() {
      if !index_map.contains_key(name) {
        strong_connect(name, &deps, &mut index, &mut index_map, &mut low_link, &mut stack, &mut components);
      }
    }
    let components = components.into_iter().map(|x| x.into_iter().collect()).collect();
    RecGroups(components)
  }
}

/* Inference, unification and type checking */
fn infer_book(book: &Book, diags: &mut Diagnostics) -> Result<ProgramTypes, Diagnostics> {
  let groups = RecGroups::from_book(book);
  let mut env = TypeEnv::default();
  // Note: We store the inferred and generalized types in a separate
  // environment, to avoid unnecessary cloning (since no immutable data).
  let mut types = ProgramTypes::default();

  // Add the constructors to the environment.
  for adt in book.adts.values() {
    for ctr in adt.ctrs.values() {
      types.insert(ctr.name.clone(), ctr.typ.generalize(&TypeEnv::default()));
    }
  }
  // Add the types of unchecked functions to the environment.
  for def in book.defs.values() {
    if !def.check {
      types.insert(def.name.clone(), def.typ.generalize(&TypeEnv::default()));
    }
  }
  // Add the types of hvm functions to the environment.
  for def in book.hvm_defs.values() {
    types.insert(def.name.clone(), def.typ.generalize(&TypeEnv::default()));
  }

  // Infer the types of regular functions.
  for group in &groups.0 {
    infer_group(&mut env, book, group, &mut types, diags)?;
  }
  Ok(types)
}

fn infer_group(
  env: &mut TypeEnv,
  book: &Book,
  group: &[Name],
  types: &mut ProgramTypes,
  diags: &mut Diagnostics,
) -> Result<(), Diagnostics> {
  let var_gen = &mut VarGen::default();
  // Generate fresh type variables for each function in the group.
  let tvs = group.iter().map(|_| var_gen.fresh()).collect::<Vec<_>>();
  for (name, tv) in group.iter().zip(tvs.iter()) {
    env.insert(name.clone(), Scheme(vec![], tv.clone()));
  }

  // Infer the types of the functions in the group.
  let mut ss = vec![];
  let mut inf_ts = vec![];
  let mut exp_ts = vec![];
  for name in group {
    let def = &book.defs[name];
    let (s, t) = infer(env, book, types, &def.rule().body, var_gen).map_err(|e| {
      diags.add_function_error(e, name.clone(), def.source.clone());
      std::mem::take(diags)
    })?;
    let t = t.subst(&s);
    ss.push(s);
    inf_ts.push(t);
    exp_ts.push(&def.typ);
  }

  // Remove the type variables of the group from the environment.
  // This avoids cloning of already generalized types.
  for name in group.iter() {
    env.0.remove(name);
  }

  // Unify the inferred body with the corresponding type variable.
  let mut s = ss.into_iter().fold(Subst::default(), |acc, s| acc.compose(s));
  let mut ts = vec![];
  for ((bod_t, tv), nam) in inf_ts.into_iter().zip(tvs.iter()).zip(group.iter()) {
    let (t, s2) = unify_term(&tv.subst(&s), &bod_t, &book.defs[nam].rule().body)?;
    ts.push(t);
    s = s.compose(s2);
  }
  let ts = ts.into_iter().map(|t| t.subst(&s)).collect::<Vec<_>>();

  // Specialize against the expected type, then generalize and store.
  for ((name, exp_t), inf_t) in group.iter().zip(exp_ts.iter()).zip(ts.iter()) {
    let t = specialize(inf_t, exp_t).map_err(|e| {
      diags.add_function_error(e, name.clone(), book.defs[name].source.clone());
      std::mem::take(diags)
    })?;
    types.insert(name.clone(), t.generalize(&TypeEnv::default()));
  }

  diags.fatal(())
}

/// Infer the type of a term in the given environment.
///
/// The type environment must contain bindings for all the free variables of the term.
///
/// The returned substitution records the type constraints imposed on type variables by the term.
/// The returned type is the type of the term.
fn infer(
  env: &mut TypeEnv,
  book: &Book,
  types: &ProgramTypes,
  term: &Term,
  var_gen: &mut VarGen,
) -> Result<(Subst, Type), String> {
  let res = maybe_grow(|| match term {
    Term::Var { nam } | Term::Ref { nam } => {
      if let Some(scheme) = env.0.get(nam) {
        Ok::<_, String>((Subst::default(), scheme.instantiate(var_gen)))
      } else if let Some(scheme) = types.get(nam) {
        Ok((Subst::default(), scheme.instantiate(var_gen)))
      } else {
        unreachable!("unbound name '{}'", nam)
      }
    }
    Term::Lam { tag: Tag::Static, pat, bod } => match pat.as_ref() {
      Pattern::Var(nam) => {
        let tv = var_gen.fresh();
        let old_bnd = env.add_binds([(nam, Scheme(vec![], tv.clone()))]);
        let (s, bod_t) = infer(env, book, types, bod, var_gen)?;
        env.pop_binds(old_bnd);
        let var_t = tv.subst(&s);
        Ok((s, Type::Arr(Box::new(var_t), Box::new(bod_t))))
      }
      _ => unreachable!("{}", term),
    },
    Term::App { tag: Tag::Static, fun, arg } => {
      let (s1, fun_t) = infer(env, book, types, fun, var_gen)?;
      let (s2, arg_t) = infer(&mut env.subst(&s1), book, types, arg, var_gen)?;
      let app_t = var_gen.fresh();
      let (_, s3) = unify_term(&fun_t.subst(&s2), &Type::Arr(Box::new(arg_t), Box::new(app_t.clone())), fun)?;
      let t = app_t.subst(&s3);
      Ok((s3.compose(s2).compose(s1), t))
    }
    Term::Let { pat, val, nxt } => match pat.as_ref() {
      Pattern::Var(nam) => {
        let (s1, val_t) = infer(env, book, types, val, var_gen)?;
        let old_bnd = env.add_binds([(nam, val_t.generalize(&env.subst(&s1)))]);
        let (s2, nxt_t) = infer(&mut env.subst(&s1), book, types, nxt, var_gen)?;
        env.pop_binds(old_bnd);
        Ok((s2.compose(s1), nxt_t))
      }
      Pattern::Fan(FanKind::Tup, Tag::Static, _) => {
        // Tuple elimination behaves like pattern matching.
        // Variables from tuple patterns don't get generalized.
        debug_assert!(!(pat.has_unscoped() || pat.has_nested()));
        let (s1, val_t) = infer(env, book, types, val, var_gen)?;

        let tvs = pat.binds().map(|_| var_gen.fresh()).collect::<Vec<_>>();
        let old_bnd = env.add_binds(pat.binds().zip(tvs.iter().map(|tv| Scheme(vec![], tv.clone()))));
        let (s2, nxt_t) = infer(&mut env.subst(&s1), book, types, nxt, var_gen)?;
        env.pop_binds(old_bnd);
        let tvs = tvs.into_iter().map(|tv| tv.subst(&s2)).collect::<Vec<_>>();
        let (_, s3) = unify_term(&val_t, &Type::Tup(tvs), val)?;
        Ok((s3.compose(s2).compose(s1), nxt_t))
      }
      Pattern::Fan(FanKind::Dup, Tag::Auto, _) => {
        // We pretend that sups don't exist and dups don't collide.
        // All variables must have the same type as the body of the dup.
        debug_assert!(!(pat.has_unscoped() || pat.has_nested()));
        let (s1, mut val_t) = infer(env, book, types, val, var_gen)?;
        let tvs = pat.binds().map(|_| var_gen.fresh()).collect::<Vec<_>>();
        let old_bnd = env.add_binds(pat.binds().zip(tvs.iter().map(|tv| Scheme(vec![], tv.clone()))));
        let (mut s2, nxt_t) = infer(&mut env.subst(&s1), book, types, nxt, var_gen)?;
        env.pop_binds(old_bnd);
        for tv in tvs {
          let (val_t_, s) = unify_term(&val_t, &tv.subst(&s2), val)?;
          val_t = val_t_;
          s2 = s2.compose(s);
        }
        Ok((s2.compose(s1), nxt_t))
      }
      _ => unreachable!(),
    },

    Term::Mat { bnd: _, arg, with_bnd: _, with_arg: _, arms } => {
      // Infer type of the scrutinee
      let (s1, t1) = infer(env, book, types, arg, var_gen)?;

      // Instantiate the expected type of the scrutinee
      let adt_name = book.ctrs.get(arms[0].0.as_ref().unwrap()).unwrap();
      let adt = &book.adts[adt_name];
      let (adt_s, adt_t) = instantiate_adt(adt, var_gen)?;

      // For each case, infer the types and unify them all.
      // Unify the inferred type of the destructured fields with the
      // expected from what we inferred from the scrutinee.
      let (s2, nxt_t) = infer_match_cases(env.subst(&s1), book, types, adt, arms, &adt_s, var_gen)?;

      // Unify the inferred type with the expected type
      let (_, s3) = unify_term(&t1, &adt_t.subst(&s2), arg)?;
      Ok((s3.compose(s2).compose(s1), nxt_t))
    }

    Term::Num { val } => {
      let t = match val {
        Num::U24(_) => Type::U24,
        Num::I24(_) => Type::I24,
        Num::F24(_) => Type::F24,
      };
      Ok((Subst::default(), t))
    }
    Term::Oper { opr, fst, snd } => {
      let (s1, t1) = infer(env, book, types, fst, var_gen)?;
      let (s2, t2) = infer(&mut env.subst(&s1), book, types, snd, var_gen)?;
      let (t2, s3) = unify_term(&t2.subst(&s1), &t1.subst(&s2), term)?;
      let s_args = s3.compose(s2).compose(s1);
      let t_args = t2.subst(&s_args);
      // Check numeric type matches the operation
      let tv = var_gen.fresh();
      let (t_opr, s_opr) = match opr {
        // Any numeric type
        Op::ADD | Op::SUB | Op::MUL | Op::DIV => {
          unify_term(&t_args, &Type::Number(Box::new(tv.clone())), term)?
        }
        Op::EQ | Op::NEQ | Op::LT | Op::GT | Op::GE | Op::LE => {
          let (_, s) = unify_term(&t_args, &Type::Number(Box::new(tv.clone())), term)?;
          (Type::U24, s)
        }
        // Integers
        Op::REM | Op::AND | Op::OR | Op::XOR | Op::SHL | Op::SHR => {
          unify_term(&t_args, &Type::Integer(Box::new(tv.clone())), term)?
        }
        // Floating
        Op::POW => unify_term(&t_args, &Type::F24, term)?,
      };
      let t = t_opr.subst(&s_opr);
      Ok((s_opr.compose(s_args), t))
    }
    Term::Swt { bnd: _, arg, with_bnd: _, with_arg: _, pred, arms } => {
      let (s1, t1) = infer(env, book, types, arg, var_gen)?;
      let (_, s2) = unify_term(&t1, &Type::U24, arg)?;
      let s_arg = s2.compose(s1);
      let mut env = env.subst(&s_arg);

      let mut ss_nums = vec![];
      let mut ts_nums = vec![];
      for arm in arms.iter().rev().skip(1) {
        let (s, t) = infer(&mut env, book, types, arm, var_gen)?;
        env = env.subst(&s);
        ss_nums.push(s);
        ts_nums.push(t);
      }
      let old_bnd = env.add_binds([(pred, Scheme(vec![], Type::U24))]);
      let (s_succ, t_succ) = infer(&mut env, book, types, &arms[1], var_gen)?;
      env.pop_binds(old_bnd);

      let s_arms = ss_nums.into_iter().fold(s_succ, |acc, s| acc.compose(s));
      let mut t_swt = t_succ;
      let mut s_swt = Subst::default();
      for t_num in ts_nums {
        let (t, s) = unify_term(&t_swt, &t_num, term)?;
        t_swt = t;
        s_swt = s.compose(s_swt);
      }

      let s = s_swt.compose(s_arms).compose(s_arg);
      let t = t_swt.subst(&s);
      Ok((s, t))
    }

    Term::Fan { fan: FanKind::Tup, tag: Tag::Static, els } => {
      let res = els.iter().map(|el| infer(env, book, types, el, var_gen)).collect::<Result<Vec<_>, _>>()?;
      let (ss, ts): (Vec<Subst>, Vec<Type>) = res.into_iter().unzip();
      let t = Type::Tup(ts);
      let s = ss.into_iter().fold(Subst::default(), |acc, s| acc.compose(s));
      Ok((s, t))
    }
    Term::Era => Ok((Subst::default(), Type::None)),
    Term::Fan { .. } | Term::Lam { tag: _, .. } | Term::App { tag: _, .. } | Term::Link { .. } => {
      unreachable!("'{term}' while type checking. Should never occur in checked functions")
    }
    Term::Use { .. }
    | Term::With { .. }
    | Term::Ask { .. }
    | Term::Nat { .. }
    | Term::Str { .. }
    | Term::List { .. }
    | Term::Fold { .. }
    | Term::Bend { .. }
    | Term::Open { .. }
    | Term::Def { .. }
    | Term::Err => unreachable!("'{term}' while type checking. Should have been removed in earlier pass"),
  })?;
  Ok(res)
}

/// Instantiates the type constructor of an ADT, also returning the
/// ADT var to instantiated var substitution, to be used when
/// instantiating the types of the fields of the eliminated constructors.
fn instantiate_adt(adt: &Adt, var_gen: &mut VarGen) -> Result<(Subst, Type), String> {
  let tvs = adt.vars.iter().map(|_| var_gen.fresh());
  let s = Subst(adt.vars.iter().zip(tvs).map(|(x, t)| (x.clone(), t)).collect());
  let t = Type::Ctr(adt.name.clone(), adt.vars.iter().cloned().map(Type::Var).collect());
  let t = t.subst(&s);
  Ok((s, t))
}

fn infer_match_cases(
  mut env: TypeEnv,
  book: &Book,
  types: &ProgramTypes,
  adt: &Adt,
  arms: &[MatchRule],
  adt_s: &Subst,
  var_gen: &mut VarGen,
) -> Result<(Subst, Type), String> {
  maybe_grow(|| {
    if let Some(((ctr_nam, vars, bod), rest)) = arms.split_first() {
      let ctr = &adt.ctrs[ctr_nam.as_ref().unwrap()];
      // One fresh var per field, we later unify with the expected type.
      let tvs = vars.iter().map(|_| var_gen.fresh()).collect::<Vec<_>>();

      // Infer the body and unify the inferred field types with the expected.
      let old_bnd = env.add_binds(vars.iter().zip(tvs.iter().map(|tv| Scheme(vec![], tv.clone()))));
      let (s1, t1) = infer(&mut env, book, types, bod, var_gen)?;
      env.pop_binds(old_bnd);
      let inf_ts = tvs.into_iter().map(|tv| tv.subst(&s1)).collect::<Vec<_>>();
      let exp_ts = ctr.fields.iter().map(|f| f.typ.subst(adt_s)).collect::<Vec<_>>();
      let s2 = unify_fields(inf_ts.iter().zip(exp_ts.iter()), bod)?;

      // Recurse and unify with the other arms.
      let s = s2.compose(s1);
      let (s_rest, t_rest) = infer_match_cases(env.subst(&s), book, types, adt, rest, adt_s, var_gen)?;
      let (t_final, s_final) = unify_term(&t1, &t_rest, bod)?;

      Ok((s_final.compose(s_rest).compose(s), t_final))
    } else {
      Ok((Subst::default(), var_gen.fresh()))
    }
  })
}

fn unify_fields<'a>(ts: impl Iterator<Item = (&'a Type, &'a Type)>, ctx: &Term) -> Result<Subst, String> {
  let ss = ts.map(|(inf, exp)| unify_term(inf, exp, ctx)).collect::<Result<Vec<_>, _>>()?;
  let mut s = Subst::default();
  for (_, s2) in ss.into_iter().rev() {
    s = s.compose(s2);
  }
  Ok(s)
}

fn unify_term(t1: &Type, t2: &Type, ctx: &Term) -> Result<(Type, Subst), String> {
  match unify(t1, t2) {
    Ok((t, s)) => Ok((t, s)),
    Err(msg) => Err(format!("In {ctx}:\n  Can't unify '{t1}' and '{t2}'.{msg}")),
  }
}

fn unify(t1: &Type, t2: &Type) -> Result<(Type, Subst), String> {
  maybe_grow(|| match (t1, t2) {
    (t, Type::Hole) | (Type::Hole, t) => Ok((t.clone(), Subst::default())),
    (t, Type::Var(x)) | (Type::Var(x), t) => {
      // Try to bind variable `x` to `t`
      if let Type::Var(y) = t {
        if y == x {
          // Don't bind a variable to itself
          return Ok((t.clone(), Subst::default()));
        }
      }
      // Occurs check
      if t.free_type_vars().contains(x) {
        return Err(format!(" Variable '{x}' occurs in '{t}'"));
      }
      Ok((t.clone(), Subst(BTreeMap::from([(x.clone(), t.clone())]))))
    }
    (Type::Arr(l1, r1), Type::Arr(l2, r2)) => {
      let (t1, s1) = unify(l1, l2)?;
      let (t2, s2) = unify(&r1.subst(&s1), &r2.subst(&s1))?;
      Ok((Type::Arr(Box::new(t1), Box::new(t2)), s2.compose(s1)))
    }
    (Type::Ctr(name1, ts1), Type::Ctr(name2, ts2)) if name1 == name2 && ts1.len() == ts2.len() => {
      let mut s = Subst::default();
      let mut ts = vec![];
      for (t1, t2) in ts1.iter().zip(ts2.iter()) {
        let (t, s2) = unify(t1, t2)?;
        ts.push(t);
        s = s.compose(s2);
      }
      Ok((Type::Ctr(name1.clone(), ts), s))
    }
    (Type::Tup(els1), Type::Tup(els2)) if els1.len() == els2.len() => {
      let mut s = Subst::default();
      let mut ts = vec![];
      for (t1, t2) in els1.iter().zip(els2.iter()) {
        let (t, s2) = unify(t1, t2)?;
        ts.push(t);
        s = s.compose(s2);
      }
      Ok((Type::Tup(ts), s))
    }
    t @ ((Type::U24, Type::U24)
    | (Type::F24, Type::F24)
    | (Type::I24, Type::I24)
    | (Type::None, Type::None)) => Ok((t.0.clone(), Subst::default())),
    (Type::Number(t1), Type::Number(t2)) => {
      let (t, s) = unify(t1, t2)?;
      Ok((Type::Number(Box::new(t)), s))
    }
    (Type::Number(tn), Type::Integer(ti)) | (Type::Integer(ti), Type::Number(tn)) => {
      let (t, s) = unify(ti, tn)?;
      Ok((Type::Integer(Box::new(t)), s))
    }
    (Type::Integer(t1), Type::Integer(t2)) => {
      let (t, s) = unify(t1, t2)?;
      Ok((Type::Integer(Box::new(t)), s))
    }
    (Type::Number(t1) | Type::Integer(t1), t2 @ (Type::U24 | Type::I24 | Type::F24))
    | (t2 @ (Type::U24 | Type::I24 | Type::F24), Type::Number(t1) | Type::Integer(t1)) => {
      let (t, s) = unify(t1, t2)?;
      Ok((t, s))
    }

    (Type::Any, t) | (t, Type::Any) => {
      let mut s = Subst::default();
      // Recurse to assign variables to `Any` as well
      for child in t.children() {
        let (_, s2) = unify(&Type::Any, child)?;
        s = s2.compose(s);
      }
      Ok((Type::Any, s))
    }

    _ => Err(String::new()),
  })
}

/// Specializes the inferred type against the type annotation.
/// This way, the annotation can be less general than the inferred type.
///
/// It also forces inferred 'Any' to the annotated, inferred types to
/// annotated 'Any' and fills 'Hole' with the inferred type.
///
/// Errors if the first type is not a superset of the second type.
fn specialize(inf: &Type, ann: &Type) -> Result<Type, String> {
  fn merge_specialization(inf: &Type, exp: &Type, s: &mut Subst) -> Result<Type, String> {
    maybe_grow(|| match (inf, exp) {
      // These rules have to come before
      (t, Type::Hole) => Ok(t.clone()),
      (Type::Hole, _) => unreachable!("Hole should never appear in the inferred type"),

      (_inf, Type::Any) => Ok(Type::Any),
      (Type::Any, exp) => Ok(exp.clone()),

      (Type::Var(x), new) => {
        if let Some(old) = s.0.get(x) {
          if old == new {
            Ok(new.clone())
          } else {
            Err(format!(" Inferred type variable '{x}' must be both '{old}' and '{new}'"))
          }
        } else {
          s.0.insert(x.clone(), new.clone());
          Ok(new.clone())
        }
      }

      (Type::Arr(l1, r1), Type::Arr(l2, r2)) => {
        let l = merge_specialization(l1, l2, s)?;
        let r = merge_specialization(r1, r2, s)?;
        Ok(Type::Arr(Box::new(l), Box::new(r)))
      }
      (Type::Ctr(name1, ts1), Type::Ctr(name2, ts2)) if name1 == name2 && ts1.len() == ts2.len() => {
        let mut ts = vec![];
        for (t1, t2) in ts1.iter().zip(ts2.iter()) {
          let t = merge_specialization(t1, t2, s)?;
          ts.push(t);
        }
        Ok(Type::Ctr(name1.clone(), ts))
      }
      (Type::Tup(ts1), Type::Tup(ts2)) if ts1.len() == ts2.len() => {
        let mut ts = vec![];
        for (t1, t2) in ts1.iter().zip(ts2.iter()) {
          let t = merge_specialization(t1, t2, s)?;
          ts.push(t);
        }
        Ok(Type::Tup(ts))
      }
      (Type::Number(t1), Type::Number(t2)) => Ok(Type::Number(Box::new(merge_specialization(t1, t2, s)?))),
      (Type::Integer(t1), Type::Integer(t2)) => Ok(Type::Integer(Box::new(merge_specialization(t1, t2, s)?))),
      (Type::U24, Type::U24) | (Type::F24, Type::F24) | (Type::I24, Type::I24) | (Type::None, Type::None) => {
        Ok(inf.clone())
      }
      _ => Err(String::new()),
    })
  }

  // Refresh the variable names to avoid conflicts when unifying
  // Names of type vars in the annotation have nothing to do with names in the inferred type.
  let var_gen = &mut VarGen::default();
  let inf2 = inf.generalize(&TypeEnv::default()).instantiate(var_gen);
  let ann2 = ann.generalize(&TypeEnv::default()).instantiate(var_gen);

  let (t, s) = unify(&inf2, &ann2)
    .map_err(|e| format!("Type Error: Expected function type '{ann}' but found '{inf}'.{e}"))?;
  let t = t.subst(&s);

  // Merge the inferred specialization with the expected type.
  // This is done to cast to/from `Any` and `_` types.
  let mut merge_s = Subst::default();
  let t2 = merge_specialization(&t, ann, &mut merge_s).map_err(|e| {
    format!("Type Error: Annotated type '{ann}' is not a subtype of inferred type '{inf2}'.{e}")
  })?;

  Ok(t2.subst(&merge_s))
}

impl std::fmt::Display for Subst {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "Subst {{")?;
    for (x, y) in &self.0 {
      writeln!(f, "  {x} => {y},")?;
    }
    write!(f, "}}")
  }
}
