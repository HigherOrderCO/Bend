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
        let substs = val.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
      Stmt::InPlace { val, nxt, .. } => {
        nxt.gen_map_get();
        let substs = val.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
      Stmt::If { cond, then, otherwise } => {
        then.gen_map_get();
        otherwise.gen_map_get();
        let substs = cond.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
      Stmt::Match { arg, arms, .. } | Stmt::Fold { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.rgt.gen_map_get();
        }
        let substs = arg.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
      Stmt::Switch { arg, arms, .. } => {
        for arm in arms.iter_mut() {
          arm.gen_map_get();
        }
        let substs = arg.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
      Stmt::Do { fun: _, block: _ } => todo!(),
      Stmt::Return { term } => {
        let substs = term.subst_map_gets();
        if !substs.is_empty() {
          *self = gen_get(self.clone(), substs);
        }
      }
    }
  }
}

impl Expression {
  fn subst_map_gets(&mut self) -> HashMap<Name, (Name, MapKey)> {
    fn go(e: &mut Expression, subst: &mut HashMap<Name, (Name, MapKey)>) {
      match e {
        Expression::MapGet { nam, key } => {
          let new_var = gen_map_var(subst);
          subst.insert(new_var.clone(), (nam.clone(), *key));
          *e = Expression::Var { nam: new_var };
        }
        Expression::Call { fun, args, kwargs } => {
          go(fun, subst);
          for arg in args {
            go(arg, subst);
          }
          for (_, arg) in kwargs {
            go(arg, subst);
          }
        }
        Expression::Lam { bod, .. } => {
          go(bod, subst);
        }
        Expression::Bin { lhs, rhs, .. } => {
          go(lhs, subst);
          go(rhs, subst);
        }
        Expression::Lst { els } | Expression::Tup { els } => {
          for el in els {
            go(el, subst);
          }
        }
        Expression::Comprehension { term, iter, cond, .. } => {
          go(term, subst);
          go(iter, subst);
          if let Some(cond) = cond {
            go(cond, subst);
          }
        }
        Expression::MapInit { entries } => {
          for (_, entry) in entries {
            go(entry, subst);
          }
        }
        Expression::None | Expression::Str { .. } | Expression::Var { .. } | Expression::Num { .. } => {}
      }
    }
    let mut subst = HashMap::new();
    go(self, &mut subst);
    subst
  }
}

fn gen_get(curr: Stmt, substs: HashMap<Name, (Name, MapKey)>) -> Stmt {
  substs.into_iter().fold(curr, |acc, next| {
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

fn gen_map_var(subst: &HashMap<Name, (Name, MapKey)>) -> Name {
  Name::new(format!("map/get%{}", subst.len()))
}
