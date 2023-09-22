use crate::ast::{hvm_lang::DefNames, DefinitionBook, Name, Term};
use std::collections::{BTreeMap, HashMap, VecDeque};

impl DefinitionBook {
  pub fn try_into_affine(&mut self) -> anyhow::Result<()> {
    for def in self.defs.iter_mut() {
      for rule in def.rules.iter_mut() {
        rule.body = rule.body.try_into_affine(&self.def_names)?;
      }
    }
    Ok(())
  }
}

impl Term {
  /// Makes sure that all variables are affine (used 0 or 1 times), adding the needed dups.
  /// Checks for variables not defined anywhere.
  /// Converts def references into a Ref type
  pub fn try_into_affine(&self, def_names: &DefNames) -> anyhow::Result<Self> {
    let mut globals = HashMap::new();
    let term = term_to_affine(self, &mut BTreeMap::new(), &mut globals, def_names)?;
    for (name, (defined, used)) in globals {
      if !defined {
        return Err(anyhow::anyhow!("Unbound global variable '${name}'"));
      }
      if !used {
        // TODO: Instead, we should transform into a scoped erased lambda
        return Err(anyhow::anyhow!("Unused global variable '${name}'"));
      }
    }
    Ok(term)
  }
}

type Scope = BTreeMap<Name, Vec<Vec<usize>>>;

/// `scope` maps variable names to a stack of set of ids per var sharing this name>.
/// `num_uses` counts how many times each variable name was used. This is used to create unique names.
/// `globals` stores if a global variable has been defined and used somewhere in the term.
fn term_to_affine(
  value: &Term,
  scope: &mut Scope,
  globals: &mut HashMap<Name, (bool, bool)>,
  def_names: &DefNames,
) -> anyhow::Result<Term> {
  let term = match value {
    Term::Lam { nam, bod } => {
      push_scope(nam, scope);
      let bod = term_to_affine(bod, scope, globals, def_names)?.into();
      let (nam, bod) = pop_scope(nam, bod, scope, def_names);
      Term::Lam { nam, bod }
    }
    Term::Var { nam } => {
      // Count this var use and give it a new unique name
      if let Some(var_use_stack) = scope.get(nam) {
        let var_uses = var_use_stack.last().unwrap().clone();
        // Create a new name, except for the first occurence
        let (new_name, name_idx) = if var_uses.is_empty() {
          (nam.clone(), 0)
        } else {
          make_new_dup_name(nam, &var_uses, scope, def_names)
        };
        // Add new name to scope
        scope.get_mut(nam).unwrap().last_mut().unwrap().push(name_idx);
        Term::Var { nam: new_name }
      } else {
        // Unbound var, could be a def, could be actually unbound
        if let Some(def_id) = def_names.get_by_right(nam) {
          Term::Ref { def_id: *def_id }
        } else {
          return Err(anyhow::anyhow!("Unbound variable '{nam}'"));
        }
      }
    }
    // TODO: Add var use checking for global lambdas and vars
    Term::GlobalLam { nam, bod } => {
      if let Some(global_use) = globals.get_mut(nam) {
        if global_use.0 {
          return Err(anyhow::anyhow!("Global variable '{nam}' declared more than once"));
        } else {
          global_use.0 = true;
        }
      }
      let bod = term_to_affine(bod, scope, globals, def_names)?.into();
      Term::GlobalLam { nam: nam.clone(), bod }
    }
    Term::GlobalVar { nam } => {
      if let Some(global_use) = globals.get_mut(nam) {
        if global_use.1 {
          // TODO: Add dups on the first outer scope that contain all uses.
          return Err(anyhow::anyhow!(
            "Global variable '{nam}' used more than once. Explicitly assign it to a scoped variable with a let or dup instead."
          ));
        } else {
          global_use.1 = true;
        }
      }
      Term::GlobalVar { nam: nam.clone() }
    }
    Term::Ref { def_id } => {
      // We expect to not encounter this case, but if something changes in the future,
      // we're already deling with the possibility.
      if def_names.contains_left(def_id) {
        Term::Ref { def_id: *def_id }
      } else {
        return Err(anyhow::anyhow!("Reference to undefined definition (Unknown name for now)"));
      }
    }
    Term::App { fun, arg } => Term::App {
      fun: Box::new(term_to_affine(fun, scope, globals, def_names)?),
      arg: Box::new(term_to_affine(arg, scope, globals, def_names)?),
    },
    // TODO: Should we add support for manually specifying sup terms?
    // Term::Sup { label, fst, snd } => Ok(Term::Sup {
    //   label,
    //   fst: term_to_affine(*fst, scope, def_names)?.into(),
    //   snd: term_to_affine(*snd, scope, def_names)?.into(),
    // }),
    Term::Dup { fst, snd, val, nxt } => {
      if fst == snd {
        if let Some(fst) = fst {
          // TODO: This should probably be a warning instead of an error
          return Err(anyhow::anyhow!("Found dup with same name for both variables: '{fst}'"));
        }
      }
      let val = term_to_affine(val, scope, globals, def_names)?.into();
      push_scope(fst, scope);
      push_scope(snd, scope);
      let nxt = term_to_affine(nxt, scope, globals, def_names)?.into();
      let (snd, nxt) = pop_scope(snd, nxt, scope, def_names);
      let (fst, nxt) = pop_scope(fst, nxt, scope, def_names);
      Term::Dup { fst, snd, val, nxt }
    }
    Term::NumOp { op, fst, snd } => Term::NumOp {
      op: *op,
      fst: Box::new(term_to_affine(fst, scope, globals, def_names)?),
      snd: Box::new(term_to_affine(snd, scope, globals, def_names)?),
    },
    num @ Term::Num { .. } => num.clone(),
    Term::Sup { .. } => unreachable!(),
    Term::Era => unreachable!(),
    a => todo!("{}", a.to_string(def_names)),
  };
  Ok(term)
}

fn push_scope(nam: &Option<Name>, scope: &mut Scope) {
  if let Some(nam) = &nam {
    scope.entry(nam.clone()).or_default().push(vec![]);
  }
}

// Removes a variable from the scope, adding dups/era when necessary
fn pop_scope(
  nam: &Option<Name>,
  bod: Box<Term>,
  scope: &mut Scope,
  def_names: &DefNames,
) -> (Option<Name>, Box<Term>) {
  if let Some(nam) = &nam {
    // Remove variable from scope, getting all the occurences
    let var_uses = scope.get_mut(nam).unwrap().pop().unwrap();
    // Add the necessary dups to make all uses affine.
    let (new_nam, bod) = add_dups_of_var(nam, bod, &var_uses, scope, def_names);
    // Remove this name from the scope if there are no variables using it.
    if scope.get(nam).unwrap().is_empty() {
      scope.remove(nam);
    }
    (new_nam, bod)
  } else {
    (None, bod)
  }
}

fn make_dup_name(nam: &str, idx: usize) -> Name {
  // The first occurence is not renamed
  let name = if idx == 0 { nam.to_string() } else { format!("{}_{}", nam, idx) };
  Name(name)
}

fn make_new_dup_name(nam: &str, var_uses: &[usize], scope: &Scope, def_names: &DefNames) -> (Name, usize) {
  let mut new_idx = var_uses.last().map_or(0, |x| x + 1);
  // NOTE: Checking if `$var_$i` is in `scope` requires checking entries where fst == $var_$i
  //   but also entries where fst == $var and indices.contain(i).
  // And checking if `$var_$i` is in defs requires having the list of definitions at this point,
  //   so this must be done after we have all definitions of all imported files.
  loop {
    let name = make_dup_name(nam, new_idx);
    if scope.contains_key(&name) || def_names.contains_right(&name) {
      new_idx += 1;
    } else {
      return (name, new_idx);
    }
  }
}

fn add_dups_of_var(
  var_name: &Name,
  mut var_body: Box<Term>,
  var_uses: &[usize],
  scope: &Scope,
  def_names: &DefNames,
) -> (Option<Name>, Box<Term>) {
  match var_uses.len() {
    0 => (None, var_body),
    1 => (Some(make_dup_name(var_name, var_uses[0])), var_body),
    _ => {
      let mut var_uses = var_uses.to_vec();
      let mut refs_to_add = VecDeque::from_iter(var_uses.iter().map(|i| make_dup_name(var_name, *i)));
      while let (Some(fst), Some(snd)) = (refs_to_add.pop_front(), refs_to_add.pop_front()) {
        let dup_name = if refs_to_add.is_empty() {
          Name(var_name.to_string()) // Topmost DUP refers to original lambda var
        } else {
          let (name, new_idx) = make_new_dup_name(var_name, &var_uses, scope, def_names);
          refs_to_add.push_back(name.clone());
          var_uses.push(new_idx);
          name.clone()
        };
        var_body = Box::new(Term::Dup {
          fst: Some(fst),
          snd: Some(snd),
          val: Term::Var { nam: dup_name }.into(),
          nxt: var_body,
        });
      }
      (Some(var_name.clone()), var_body)
    }
  }
}
