use crate::fun::Name;

use super::{AssignPattern, Definition, Expr, Stmt};

impl Definition {
  /// Generates a map from `Stmt` to `Substitutions` for each definition in the program.
  /// Iterates over all definitions in the program and applies `gen_map_get` to their bodies.
  /// It replaces `Expr::MapGet` expressions with variable accesses, introducing
  /// new variables as necessary to hold intermediate results from map accesses.
  pub fn gen_map_get(&mut self) {
    self.body.gen_map_get(&mut 0);
  }
}

impl Stmt {
  fn gen_map_get(&mut self, id: &mut usize) {
    match self {
      Stmt::LocalDef { def, nxt } => {
        nxt.gen_map_get(id);
        def.gen_map_get()
      }
      Stmt::Assign { pat, val, nxt } => {
        let key_substitutions =
          if let AssignPattern::MapSet(_, key) = pat { key.substitute_map_gets(id) } else { Vec::new() };

        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }

        let substitutions = val.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }

        if !key_substitutions.is_empty() {
          *self = gen_get(self, key_substitutions);
        }
      }
      Stmt::Ask { pat: _, val, nxt } => {
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
        let substitutions = val.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::InPlace { op: _, pat, val, nxt } => {
        let key_substitutions = if let AssignPattern::MapSet(_, key) = &mut **pat {
          key.substitute_map_gets(id)
        } else {
          Vec::new()
        };

        nxt.gen_map_get(id);

        let substitutions = val.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }

        if !key_substitutions.is_empty() {
          *self = gen_get(self, key_substitutions);
        }
      }
      Stmt::If { cond, then, otherwise, nxt } => {
        then.gen_map_get(id);
        otherwise.gen_map_get(id);
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
        let substitutions = cond.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::Match { bnd: _, arg, with_bnd: _, with_arg, arms, nxt }
      | Stmt::Fold { bnd: _, arg, arms, with_bnd: _, with_arg, nxt } => {
        for arm in arms.iter_mut() {
          arm.rgt.gen_map_get(id);
        }
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
        let mut substitutions = arg.substitute_map_gets(id);
        for arg in with_arg {
          substitutions.extend(arg.substitute_map_gets(id));
        }
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::Switch { bnd: _, arg, with_bnd: _, with_arg, arms, nxt } => {
        for arm in arms.iter_mut() {
          arm.gen_map_get(id);
        }
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
        let mut substitutions = arg.substitute_map_gets(id);
        for arg in with_arg {
          substitutions.extend(arg.substitute_map_gets(id));
        }
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::Bend { bnd: _, arg: init, cond, step, base, nxt } => {
        step.gen_map_get(id);
        base.gen_map_get(id);
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
        let mut substitutions = cond.substitute_map_gets(id);
        for init in init {
          substitutions.extend(init.substitute_map_gets(id));
        }
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::With { typ: _, bod, nxt } => {
        bod.gen_map_get(id);
        if let Some(nxt) = nxt {
          nxt.gen_map_get(id);
        }
      }
      Stmt::Return { term } => {
        let substitutions = term.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::Open { typ: _, var: _, nxt } => {
        nxt.gen_map_get(id);
      }
      Stmt::Use { nam: _, val: bod, nxt } => {
        nxt.gen_map_get(id);
        let substitutions = bod.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self, substitutions);
        }
      }
      Stmt::Err => {}
    }
  }
}

type Substitutions = Vec<(Name, Name, Box<Expr>)>;

impl Expr {
  fn substitute_map_gets(&mut self, id: &mut usize) -> Substitutions {
    fn go(e: &mut Expr, substitutions: &mut Substitutions, id: &mut usize) {
      match e {
        Expr::MapGet { nam, key } => {
          go(key, substitutions, id);
          let new_var = gen_map_var(id);
          substitutions.push((new_var.clone(), nam.clone(), key.clone()));
          *e = Expr::Var { nam: new_var };
        }
        Expr::Call { fun, args, kwargs } => {
          go(fun, substitutions, id);
          for arg in args {
            go(arg, substitutions, id);
          }
          for (_, arg) in kwargs {
            go(arg, substitutions, id);
          }
        }
        Expr::Lam { bod, .. } => {
          go(bod, substitutions, id);
        }
        Expr::Opr { lhs, rhs, .. } => {
          go(lhs, substitutions, id);
          go(rhs, substitutions, id);
        }
        Expr::Lst { els } | Expr::Tup { els } | Expr::Sup { els } => {
          for el in els {
            go(el, substitutions, id);
          }
        }
        Expr::Ctr { kwargs, .. } => {
          for (_, arg) in kwargs.iter_mut() {
            go(arg, substitutions, id);
          }
        }
        Expr::LstMap { term, iter, cond, .. } => {
          go(term, substitutions, id);
          go(iter, substitutions, id);
          if let Some(cond) = cond {
            go(cond, substitutions, id);
          }
        }
        Expr::Map { entries } => {
          for (_, entry) in entries {
            go(entry, substitutions, id);
          }
        }
        Expr::TreeNode { left, right } => {
          go(left, substitutions, id);
          go(right, substitutions, id);
        }
        Expr::TreeLeaf { val } => {
          go(val, substitutions, id);
        }
        Expr::Era | Expr::Str { .. } | Expr::Var { .. } | Expr::Chn { .. } | Expr::Num { .. } => {}
      }
    }
    let mut substitutions = Substitutions::new();
    go(self, &mut substitutions, id);
    substitutions
  }
}

fn gen_get(current: &mut Stmt, substitutions: Substitutions) -> Stmt {
  substitutions.into_iter().rfold(std::mem::take(current), |acc, next| {
    let (var, map_var, key) = next;
    let map_get_call = Expr::Var { nam: Name::new("Map/get") };
    let map_get_call = Expr::Call {
      fun: Box::new(map_get_call),
      args: vec![Expr::Var { nam: map_var.clone() }, *key],
      kwargs: Vec::new(),
    };
    let pat = AssignPattern::Tup(vec![AssignPattern::Var(var), AssignPattern::Var(map_var)]);

    Stmt::Assign { pat, val: Box::new(map_get_call), nxt: Some(Box::new(acc)) }
  })
}

fn gen_map_var(id: &mut usize) -> Name {
  let name = Name::new(format!("map/get%{}", id));
  *id += 1;
  name
}
