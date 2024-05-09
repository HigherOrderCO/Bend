use std::collections::HashMap;

use crate::fun::Name;

use super::{AssignPattern, Expression, MapKey, Program, Stmt};

impl Program {
  pub fn gen_map_get(&mut self) {
    for def in self.defs.values_mut() {
      def.body.gen_map_get();
    }
  }
}

impl Stmt {
  fn gen_map_get(&mut self) {
    match self {
      Stmt::Assign { val, nxt, .. } => {
        nxt.gen_map_get();
        let substitutions = val.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::InPlace { val, nxt, .. } => {
        nxt.gen_map_get();
        let substitutions = val.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::If { cond, then, otherwise } => {
        then.gen_map_get();
        otherwise.gen_map_get();
        let substitutions = cond.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Match { arg, arms, .. } | Stmt::Fold { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.rgt.gen_map_get();
        }
        let substitutions = arg.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Switch { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.gen_map_get();
        }
        let substitutions = arg.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
      Stmt::Do { fun: _, block: _ } => todo!(),
      Stmt::Return { term } => {
        let substitutions = term.substitute_map_gets();
        if !substitutions.is_empty() {
          *self = gen_get(self.clone(), substitutions);
        }
      }
    }
  }
}

impl Expression {
  fn substitute_map_gets(&mut self) -> HashMap<Name, (Name, MapKey)> {
    fn go(e: &mut Expression, substitutions: &mut HashMap<Name, (Name, MapKey)>) {
      match e {
        Expression::MapGet { nam, key } => {
          let new_var = gen_map_var(substitutions);
          substitutions.insert(new_var.clone(), (nam.clone(), *key));
          *e = Expression::Var { nam: new_var };
        }
        Expression::Call { fun, args, kwargs } => {
          go(fun, substitutions);
          for arg in args {
            go(arg, substitutions);
          }
          for (_, arg) in kwargs {
            go(arg, substitutions);
          }
        }
        Expression::Lam { bod, .. } => {
          go(bod, substitutions);
        }
        Expression::Bin { lhs, rhs, .. } => {
          go(lhs, substitutions);
          go(rhs, substitutions);
        }
        Expression::Lst { els } | Expression::Tup { els } => {
          for el in els {
            go(el, substitutions);
          }
        }
        Expression::Comprehension { term, iter, cond, .. } => {
          go(term, substitutions);
          go(iter, substitutions);
          if let Some(cond) = cond {
            go(cond, substitutions);
          }
        }
        Expression::MapInit { entries } => {
          for (_, entry) in entries {
            go(entry, substitutions);
          }
        }
        Expression::None | Expression::Str { .. } | Expression::Var { .. } | Expression::Num { .. } => {}
      }
    }
    let mut substitutions = HashMap::new();
    go(self, &mut substitutions);
    substitutions
  }
}

fn gen_get(current: Stmt, substitutions: HashMap<Name, (Name, MapKey)>) -> Stmt {
  substitutions.into_iter().fold(current, |acc, next| {
    let (var, (map_var, key)) = next;
    let map_get_call = Expression::Var { nam: Name::new("Map/get") };
    let map_get_call = Expression::Call {
      fun: Box::new(map_get_call),
      args: vec![Expression::Var { nam: map_var.clone() }, Expression::Num { val: key.0 }],
      kwargs: Vec::new(),
    };
    let pat = AssignPattern::Tup(vec![var, map_var]);

    Stmt::Assign { pat, val: Box::new(map_get_call), nxt: Box::new(acc) }
  })
}

fn gen_map_var(substitutions: &HashMap<Name, (Name, MapKey)>) -> Name {
  Name::new(format!("map/get%{}", substitutions.len()))
}
