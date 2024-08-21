//! Hindley-Milner-like type system.
//!
//! Based on https://github.com/developedby/algorithm-w-rs
//! and https://github.com/mgrabmueller/AlgorithmW.
use crate::{
  fun::{Adt, Book, Constructors, Ctx, MatchRule, Name, Pattern, Tag, Term, Type},
  maybe_grow,
};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

impl Ctx<'_> {
  pub fn type_check(&mut self) -> Result<(), String> {
    eprintln!("book:\n{}", self.book);
    eprintln!("ctrs:\n{:?}", self.book.adts);
    let types = infer_book(self.book)?;
    let types = refresh_vars(types);

    for (nam, typ) in types.0 {
      eprintln!("{nam}: {typ}");
    }
    Ok(())
  }
}

#[derive(Default)]
pub struct ProgramTypes(BTreeMap<Name, Type>);

/// A type scheme, aka a polymorphic type.
#[derive(Clone)]
struct Scheme(Vec<Name>, Type);

/// A finite mapping from type variables to types.
#[derive(Clone, Default, Debug)]
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
      Type::Any => todo!(),
      Type::Hole => todo!(),
      Type::Var(x) => BTreeSet::from([x.clone()]),
      Type::Arr(t1, t2) => t1.free_type_vars().union(&t2.free_type_vars()).cloned().collect(),
      Type::Ctr(_, ts) => ts.iter().flat_map(|t| t.free_type_vars()).collect(),
      Type::Tup(_) => todo!(),
      Type::U24 => todo!(),
      Type::F24 => todo!(),
      Type::I24 => todo!(),
      Type::None => todo!(),
    })
  }

  fn subst(&self, subst: &Subst) -> Type {
    maybe_grow(|| match self {
      Type::Var(nam) => match subst.0.get(nam) {
        Some(new) => new.clone(),
        None => self.clone(),
      },
      Type::Arr(t1, t2) => Type::Arr(Box::new(t1.subst(subst)), Box::new(t2.subst(subst))),
      Type::Ctr(name, ts) => Type::Ctr(name.clone(), ts.iter().map(|t| t.subst(subst)).collect()),
      Type::Any => todo!(),
      Type::Hole => todo!(),
      Type::Tup(_) => todo!(),
      Type::U24 => todo!(),
      Type::F24 => todo!(),
      Type::I24 => todo!(),
      Type::None => todo!(),
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
  fn compose(&self, other: &Subst) -> Subst {
    let other = other.0.iter().map(|(x, t)| (x.clone(), t.subst(self)));
    let subst = self.0.iter().map(|(x, t)| (x.clone(), t.clone())).chain(other).collect();
    Subst(subst)
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

  fn append(&self, name: &Name, scheme: Scheme) -> TypeEnv {
    let mut env = self.0.clone();
    env.insert(name.clone(), scheme);
    TypeEnv(env)
  }
}

impl VarGen {
  fn fresh(&mut self) -> Type {
    let x = format!("a{}", self.0);
    self.0 += 1;
    Type::Var(Name::new(x))
  }
}

impl RecGroups {
  fn from_book(book: &Book) -> RecGroups {
    type DependencyGraph<'a> = HashMap<&'a Name, HashSet<&'a Name>>;

    fn collect_dependencies<'a>(
      term: &'a Term,
      ctrs: &'a Constructors,
      scope: &mut Vec<Name>,
      deps: &mut HashSet<&'a Name>,
    ) {
      if let Term::Ref { nam } = term {
        if ctrs.contains_key(nam) {
          // Don't infer types for constructors
        } else {
          deps.insert(nam);
        }
      }
      for (child, binds) in term.children_with_binds() {
        scope.extend(binds.clone().flatten().cloned());
        collect_dependencies(child, ctrs, scope, deps);
        scope.truncate(scope.len() - binds.flatten().count());
      }
    }

    fn strong_connect<'a>(
      v: &'a Name,
      deps: &DependencyGraph<'a>,
      index: &mut usize,
      index_map: &mut HashMap<&'a Name, usize>,
      low_link: &mut HashMap<&'a Name, usize>,
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
      if book.ctrs.contains_key(name) {
        // Don't infer types for constructors
        continue;
      }
      let mut fn_deps = Default::default();
      collect_dependencies(&def.rule().body, &book.ctrs, &mut vec![], &mut fn_deps);
      deps.insert(name, fn_deps);
    }

    // Run Tarjan's algorithm
    let mut index = 0;
    let mut stack = Vec::new();
    let mut index_map = HashMap::new();
    let mut low_link = HashMap::new();
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

fn infer_book(book: &Book) -> Result<ProgramTypes, String> {
  let rec_groups = RecGroups::from_book(book);
  let mut env = TypeEnv::default();
  let mut types = BTreeMap::new();
  // Add the constructors to the environment.
  for adt in book.adts.values() {
    for ctr in adt.ctrs.values() {
      let scheme = ctr.typ.generalize(&TypeEnv::default());
      env.0.insert(ctr.name.clone(), scheme);
      types.insert(ctr.name.clone(), ctr.typ.clone());
    }
  }
  // Infer the types of regular functions.
  let fn_ts = infer_fns(env, book, rec_groups.0.into_iter(), &mut VarGen::default())?;
  types.extend(fn_ts);
  Ok(ProgramTypes(types))
}

fn infer_fns(
  mut env: TypeEnv,
  book: &Book,
  mut groups: impl Iterator<Item = Vec<Name>>,
  var_gen: &mut VarGen,
) -> Result<Vec<(Name, Type)>, String> {
  maybe_grow(|| {
    if let Some(group) = groups.next() {
      // Generate fresh type variables for each function in the group.
      let tvs = group.iter().map(|_| var_gen.fresh()).collect::<Vec<_>>();
      for (name, tv) in group.iter().zip(tvs.iter()) {
        env.0.insert(name.clone(), Scheme(vec![], tv.clone()));
      }

      // Infer the types of the functions in the group.
      let mut ss = vec![];
      let mut ts = vec![];
      for name in &group {
        let def = book.defs.get(name).unwrap();
        let (s, t) = infer(&env, book, &def.rule().body, var_gen)?;
        ss.push(s);
        ts.push(t);
      }

      // Unify the inferred body with the corresponding type variable.
      let mut group_s = ss.into_iter().fold(Subst::default(), |s, s2| s.compose(&s2));
      for (bod_t, tv) in ts.into_iter().zip(tvs.iter()) {
        let s = unify(&tv.subst(&group_s), &bod_t)?;
        group_s = s.compose(&group_s);
      }

      // Generalize the function types
      let mut env = env.subst(&group_s);
      let final_ts = tvs.into_iter().map(|tv| tv.subst(&group_s)).collect::<Vec<_>>();
      for (name, t) in group.iter().zip(final_ts.iter()) {
        env.0.insert(name.clone(), t.generalize(&env));
      }

      let rest_ts = infer_fns(env, book, groups, var_gen)?;
      let mut program_types = group.into_iter().zip(final_ts).collect::<Vec<_>>();
      program_types.extend(rest_ts);
      Ok(program_types)
    } else {
      Ok(vec![])
    }
  })
}

/// Infer the type of a term in the given environment.
///
/// The type environment must contain bindings for all the free variables of the term.
///
/// The returned substitution records the type constraints imposed on type variables by the term.
/// The returned type is the type of the term.
fn infer(env: &TypeEnv, book: &Book, term: &Term, var_gen: &mut VarGen) -> Result<(Subst, Type), String> {
  maybe_grow(|| match term {
    Term::Var { nam } | Term::Ref { nam } => match env.0.get(nam) {
      Some(scheme) => {
        let t = scheme.instantiate(var_gen);
        Ok((Subst::default(), t))
      }
      None => unreachable!("unbound name '{}'", nam),
    },
    Term::Lam { tag: Tag::Static, pat, bod } => match pat.as_ref() {
      Pattern::Var(nam) => {
        let tv = var_gen.fresh();
        let env = if let Some(nam) = nam { env.append(nam, Scheme(vec![], tv.clone())) } else { env.clone() };
        let (s, bod_t) = infer(&env, book, bod, var_gen)?;
        let var_t = tv.subst(&s);
        Ok((s, Type::Arr(Box::new(var_t), Box::new(bod_t))))
      }
      _ => unreachable!(),
    },
    Term::App { tag: Tag::Static, fun, arg } => {
      let (s1, fun_t) = infer(env, book, fun, var_gen)?;
      let (s2, arg_t) = infer(&env.subst(&s1), book, arg, var_gen)?;
      let app_t = var_gen.fresh();
      let s3 = unify(&fun_t.subst(&s2), &Type::Arr(Box::new(arg_t), Box::new(app_t.clone())))?;
      Ok((s3.compose(&s2).compose(&s1), app_t.subst(&s3)))
    }
    Term::Let { pat, val, nxt } => match pat.as_ref() {
      Pattern::Var(nam) => {
        let (s1, val_t) = infer(env, book, val, var_gen)?;
        let nxt_env =
          if let Some(nam) = nam { env.append(nam, val_t.generalize(&env.subst(&s1))) } else { env.clone() };
        let (s2, nxt_t) = infer(&nxt_env, book, nxt, var_gen)?;
        Ok((s2.compose(&s1), nxt_t))
      }
      _ => todo!(),
    },
    Term::Mat { bnd: _, arg, with_bnd: _, with_arg: _, arms } => {
      // Infer type of the scrutinee
      let (s1, t1) = infer(env, book, arg, var_gen)?;

      // Instantiate the expected type of the scrutinee
      let adt_name = book.ctrs.get(arms[0].0.as_ref().unwrap()).unwrap();
      let adt = &book.adts[adt_name];
      let (s2, arg_t) = instantiate_adt(adt, var_gen)?;

      // Unify the inferred type with the expected type
      let s3 = unify(&t1.subst(&s2), &arg_t)?;

      // For each case, infer the types and unify them all.
      // Unify the inferred type of the destructured fields with the
      // expected from what we inferred from the scrutinee.
      let s = s3.compose(&s2).compose(&s1);
      let env = env.subst(&s);
      infer_match_cases(&env, book, adt, arms, &s, var_gen)
    }
    Term::Lam { tag: _, .. } => todo!(),
    Term::App { tag: _, .. } => todo!(),
    Term::Link { .. } => todo!(),
    Term::Use { .. } => todo!(),
    Term::Fan { .. } => todo!(),
    Term::Num { .. } => todo!(),
    Term::Oper { .. } => todo!(),
    Term::Swt { .. } => todo!(),
    Term::With { .. }
    | Term::Ask { .. }
    | Term::Nat { .. }
    | Term::Str { .. }
    | Term::List { .. }
    | Term::Fold { .. }
    | Term::Bend { .. }
    | Term::Open { .. }
    | Term::Def { .. }
    | Term::Era
    | Term::Err => unreachable!(),
  })
}

fn instantiate_adt(adt: &Adt, var_gen: &mut VarGen) -> Result<(Subst, Type), String> {
  let tvs = adt.vars.iter().map(|_| var_gen.fresh());
  let s = Subst(adt.vars.iter().zip(tvs).map(|(x, t)| (x.clone(), t)).collect());
  let t = Type::Ctr(adt.name.clone(), adt.vars.iter().cloned().map(Type::Var).collect());
  let t = t.subst(&s);
  Ok((s, t))
}

fn infer_match_cases(
  env: &TypeEnv,
  book: &Book,
  adt: &Adt,
  arms: &[MatchRule],
  s: &Subst,
  var_gen: &mut VarGen,
) -> Result<(Subst, Type), String> {
  maybe_grow(|| {
    if let Some(((ctr_nam, vars, bod), rest)) = arms.split_first() {
      let ctr = &adt.ctrs[ctr_nam.as_ref().unwrap()];
      // One fresh var per field, we later unify with the expected type.
      let tvs = vars.iter().map(|_| var_gen.fresh()).collect::<Vec<_>>();

      // Add the fields to the environment.
      let mut case_env = env.clone();
      for (var, tv) in vars.iter().zip(tvs.iter()) {
        case_env.0.insert(var.as_ref().unwrap().clone(), Scheme(vec![], tv.clone()));
      }

      // Infer the body and unify the inferred field types with the expected.
      let (s1, t1) = infer(&case_env, book, bod, var_gen)?;
      let inf_ts = tvs.into_iter().map(|tv| tv.subst(&s1)).collect::<Vec<_>>();
      let exp_ts = ctr.fields.iter().map(|f| f.typ.subst(s)).collect::<Vec<_>>();
      let s2 = unify_fields(inf_ts.iter().zip(exp_ts.iter()))?;

      // Recurse and unify with the other arms.
      let (s_rest, t_rest) = infer_match_cases(env, book, adt, rest, s, var_gen)?;
      let final_s = unify(&t1.subst(&s_rest.compose(&s2)), &t_rest)?;

      Ok((final_s.compose(&s_rest).compose(&s2).compose(&s1).compose(s), t_rest.subst(&final_s)))
    } else {
      let t = var_gen.fresh().subst(s);
      Ok((s.clone(), t))
    }
  })
}

fn unify_fields<'a>(ts: impl Iterator<Item = (&'a Type, &'a Type)>) -> Result<Subst, String> {
  let ss = ts.map(|(inf, exp)| unify(inf, exp)).collect::<Result<Vec<_>, _>>()?;
  let mut s = Subst::default();
  for s2 in ss.into_iter().rev() {
    s = s.compose(&s2);
  }
  Ok(s)
}

fn unify(t1: &Type, t2: &Type) -> Result<Subst, String> {
  maybe_grow(|| match (t1, t2) {
    (Type::Arr(l1, r1), Type::Arr(l2, r2)) => {
      let s1 = unify(l1, l2)?;
      let s2 = unify(&r1.subst(&s1), &r2.subst(&s1))?;
      Ok(s2.compose(&s1))
    }
    (t, Type::Var(x)) | (Type::Var(x), t) => {
      // Try to bind variable `x` to `t`
      if let Type::Var(y) = t {
        if y == x {
          // Don't bind a variable to itself
          return Ok(Subst::default());
        }
      }
      // Occurs check
      if t.free_type_vars().contains(x) {
        return Err(format!(
          "Cannot unify variable '{x}' with type '{t}' because it occurs as a free variable"
        ));
      }
      Ok(Subst(BTreeMap::from([(x.clone(), t.clone())])))
    }
    (Type::Ctr(name1, ts1), Type::Ctr(name2, ts2)) if name1 == name2 && ts1.len() == ts2.len() => {
      let mut s = Subst::default();
      for (t1, t2) in ts1.iter().zip(ts2.iter()) {
        s = s.compose(&unify(t1, t2)?);
      }
      Ok(s)
    }
    _ => Err(format!("Types do not unify: '{t1}' and '{t2}'")),
  })
}

fn refresh_vars(types: ProgramTypes) -> ProgramTypes {
  let mut new_types = BTreeMap::new();
  for (name, mut typ) in types.0 {
    refresh_vars_go(&mut typ, &mut BTreeMap::new(), &mut VarGen::default());
    new_types.insert(name, typ);
  }
  ProgramTypes(new_types)
}

fn refresh_vars_go(typ: &mut Type, map: &mut BTreeMap<Name, Type>, var_gen: &mut VarGen) {
  maybe_grow(|| match typ {
    Type::Var(x) => {
      if let Some(y) = map.get(x) {
        *typ = y.clone();
      } else {
        let y = var_gen.fresh();
        map.insert(x.clone(), y.clone());
        *typ = y;
      }
    }
    Type::Arr(t1, t2) => {
      refresh_vars_go(t1, map, var_gen);
      refresh_vars_go(t2, map, var_gen);
    }
    Type::Ctr(_, ts) => {
      for t in ts {
        refresh_vars_go(t, map, var_gen);
      }
    }
    Type::Any => todo!(),
    Type::Hole => todo!(),
    Type::Tup(_) => todo!(),
    Type::U24 => todo!(),
    Type::F24 => todo!(),
    Type::I24 => todo!(),
    Type::None => todo!(),
  })
}
