use std::collections::HashMap;

use crate::fun::Name;

use super::{AssignPattern, Expr, MapKey, Program, Stmt};

impl Program {
  pub fn gen_map_get(&mut self) {
    for def in self.defs.values_mut() {
      def.body.gen_map_get(&mut 0);
    }
  }
}

impl Stmt {
  fn gen_map_get(&mut self, id: &mut usize) {
    match self {
      Stmt::Assign { val, nxt, .. } => {
        nxt.gen_map_get(id);
        let substitutions = val.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::InPlace { val, nxt, .. } => {
        nxt.gen_map_get(id);
        let substitutions = val.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::If { cond, then, otherwise } => {
        then.gen_map_get(id);
        otherwise.gen_map_get(id);
        let substitutions = cond.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Match { arg, arms, .. } | Stmt::Fold { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.rgt.gen_map_get(id);
        }
        let substitutions = arg.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Switch { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.gen_map_get(id);
        }
        let substitutions = arg.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Bend { .. } => todo!(),
      Stmt::Do { fun: _, block: _ } => todo!(),
      Stmt::Return { term } => {
        let substitutions = term.substitute_map_gets(id);
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
    }
  }
}

impl Expr {
  fn substitute_map_gets(&mut self, id: &mut usize) -> HashMap<Name, (Name, MapKey)> {
    fn go(e: &mut Expr, substitutions: &mut HashMap<Name, (Name, MapKey)>, id: &mut usize) {
      match e {
        Expr::MapGet { nam, key } => {
          let new_var = gen_map_var(id);
          substitutions.insert(new_var.clone(), (nam.clone(), *key));
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
        Expr::Bin { lhs, rhs, .. } => {
          go(lhs, substitutions, id);
          go(rhs, substitutions, id);
        }
        Expr::Lst { els } | Expr::Tup { els } => {
          for el in els {
            go(el, substitutions, id);
          }
        }
        Expr::Comprehension { term, iter, cond, .. } => {
          go(term, substitutions, id);
          go(iter, substitutions, id);
          if let Some(cond) = cond {
            go(cond, substitutions, id);
          }
        }
        Expr::MapInit { entries } => {
          for (_, entry) in entries {
            go(entry, substitutions, id);
          }
        }
        Expr::None | Expr::Str { .. } | Expr::Var { .. } | Expr::Num { .. } => {}
      }
    }
    let mut substitutions = HashMap::new();
    go(self, &mut substitutions, id);
    substitutions
  }
}

fn gen_get(current: Stmt, substitutions: HashMap<Name, (Name, MapKey)>) -> Stmt {
  substitutions.into_iter().fold(current, |acc, next| {
    let (var, (map_var, key)) = next;
    let map_get_call = Expr::Var { nam: Name::new("Map/get") };
    let map_get_call = Expr::Call {
      fun: Box::new(map_get_call),
      args: vec![Expr::Var { nam: map_var.clone() }, Expr::Num { val: key.0 }],
      kwargs: Vec::new(),
    };
    let pat = AssignPattern::Tup(vec![var, map_var]);

    Stmt::Assign { pat, val: Box::new(map_get_call), nxt: Box::new(acc) }
  })
}

fn gen_map_var(id: &mut usize) -> Name {
  let name = Name::new(format!("map/get%{}", id));
  *id += 1;
  name
}
