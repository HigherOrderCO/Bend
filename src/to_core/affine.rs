use crate::ast::{Name, Term};
use std::collections::{BTreeMap, HashSet, VecDeque};

type Scope = BTreeMap<Name, Vec<Vec<usize>>>;

impl Term {
  /// Makes sure that all variables are affine (used 0 or 1 times), adding the needed dups.
  /// Checks for variables not defined anywhere.
  pub fn try_into_affine(self, def_names: &HashSet<Name>) -> anyhow::Result<Self> {
    term_to_affine(self, &mut BTreeMap::new(), def_names)
  }
}

fn make_dup_name(nam: &str, idx: usize) -> Name {
  // The first occurence is not renamed
  let name = if idx == 0 { nam.to_string() } else { format!("{}_{}", nam, idx) };
  Name(name)
}

fn make_new_dup_name(
  nam: &str,
  var_uses: &[usize],
  scope: &Scope,
  def_names: &HashSet<Name>,
) -> (Name, usize) {
  let mut new_idx = var_uses.last().map_or(0, |x| x + 1);
  // NOTE: Checking if `$var_$i` is in `scope` requires checking entries where fst == $var_$i
  //   but also entries where fst == $var and indices.contain(i).
  // And checking if `$var_$i` is in defs requires having the list of definitions at this point,
  //   so this must be done after we have all definitions of all imported files.
  loop {
    let name = make_dup_name(nam, new_idx);
    if scope.contains_key(&name) || def_names.contains(&name) {
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
  def_names: &HashSet<Name>,
) -> (Name, Box<Term>) {
  if var_uses.len() == 1 {
    (make_dup_name(var_name, var_uses[0]), var_body)
  } else {
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
      var_body = Box::new(Term::Dup { fst, snd, val: Term::Var { nam: dup_name }.into(), nxt: var_body });
    }
    (var_name.clone(), var_body)
  }
}

/// `scope` maps variable names to a stack of set of ids per var sharing this name>.
/// `num_uses` counts how many times each variable name was used. This is used to create unique names.
fn term_to_affine(value: Term, scope: &mut Scope, def_names: &HashSet<Name>) -> anyhow::Result<Term> {
  match value {
    Term::Lam { nam, bod } => {
      scope.entry(nam.clone()).or_default().push(vec![]);
      let bod = term_to_affine(*bod, scope, def_names)?.into();
      let var_uses = scope.get_mut(&*nam).unwrap().pop().unwrap();
      let (nam, bod) = add_dups_of_var(&nam, bod, &var_uses, scope, def_names);
      if scope.get(&*nam).unwrap().is_empty() {
        scope.remove(&*nam);
      }
      Ok(Term::Lam { nam, bod })
    }
    Term::Var { nam } => {
      // Count this var use and give it a new unique name
      if let Some(mut var_use_stack) = scope.remove(&*nam) {
        let var_uses = var_use_stack.last_mut().unwrap();

        // Create a new name, except for the first occurence
        let (new_name, name_idx) = if var_uses.is_empty() {
          (nam.clone(), 0)
        } else {
          make_new_dup_name(&nam, var_uses, scope, def_names)
        };
        var_uses.push(name_idx);

        // Removing and adding back to avoid ownership issues
        scope.insert(nam, var_use_stack);
        let term = Term::Var { nam: new_name };
        Ok(term)
      } else {
        // Unbound var, could be a def, could be actually unbound
        if def_names.contains(&nam) {
          Ok(Term::Var { nam })
        } else {
          Err(anyhow::anyhow!("Unbound variable '{nam}'"))
        }
      }
    }
    Term::App { fun, arg } => Ok(Term::App {
      fun: Box::new(term_to_affine(*fun, scope, def_names)?),
      arg: Box::new(term_to_affine(*arg, scope, def_names)?),
    }),
    // TODO: Should we add support for manually specifying sup terms?
    // Term::Sup { label, fst, snd } => Ok(Term::Sup {
    //   label,
    //   fst: term_to_affine(*fst, scope, def_names)?.into(),
    //   snd: term_to_affine(*snd, scope, def_names)?.into(),
    // }),
    Term::Dup { fst, snd, val, nxt } => {
      if fst == snd {
        // TODO: This should probably be a warning instead of an error
        return Err(anyhow::anyhow!("Found dup with same name for both variables: '{fst}'"));
      }
      let val = term_to_affine(*val, scope, def_names)?.into();

      scope.entry(fst.clone()).or_default().push(vec![]);
      scope.entry(snd.clone()).or_default().push(vec![]);
      let nxt = term_to_affine(*nxt, scope, def_names)?.into();
      let snd_uses = scope.get_mut(&snd).unwrap().pop().unwrap();
      let fst_uses = scope.get_mut(&fst).unwrap().pop().unwrap();
      let (snd, nxt) = add_dups_of_var(&snd, nxt, &snd_uses, scope, def_names);
      let (fst, nxt) = add_dups_of_var(&fst, nxt, &fst_uses, scope, def_names);
      Ok(Term::Dup { fst, snd, val, nxt })
    }
    num @ Term::Num { .. } => Ok(num),
    Term::NumOp { op, fst, snd } => Ok(Term::NumOp {
      op,
      fst: Box::new(term_to_affine(*fst, scope, def_names)?),
      snd: Box::new(term_to_affine(*snd, scope, def_names)?),
    }),
    Term::Sup { .. } => unreachable!(),
    Term::Era => unreachable!(),
  }
}
