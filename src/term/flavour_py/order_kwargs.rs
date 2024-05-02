use indexmap::IndexMap;

use crate::term::Name;

use super::{Enum, Program, Stmt, Term};

impl Program {
  pub fn order_kwargs(&mut self) {
    for def in self.defs.values_mut() {
      def.body.order_kwargs(&self.variants, &self.enums);
    }
  }
}

impl Stmt {
  fn order_kwargs(&mut self, variants: &IndexMap<Name, Name>, enums: &IndexMap<Name, Enum>) {
    match self {
      Stmt::Assign { val, nxt, .. } => {
        val.order_kwargs(variants, enums);
        nxt.order_kwargs(variants, enums);
      }
      Stmt::InPlace { val, nxt, .. } => {
        val.order_kwargs(variants, enums);
        nxt.order_kwargs(variants, enums);
      }
      Stmt::If { cond, then, otherwise } => {
        cond.order_kwargs(variants, enums);
        then.order_kwargs(variants, enums);
        otherwise.order_kwargs(variants, enums);
      }
      Stmt::Match { arg, arms, .. } => {
        arg.order_kwargs(variants, enums);
        for arm in arms {
          arm.rgt.order_kwargs(variants, enums);
        }
      }
      Stmt::Switch { .. } => unimplemented!(),
      Stmt::Fold { .. } => unimplemented!(),
      Stmt::Do { .. } => unimplemented!(),
      Stmt::Return { term } => term.order_kwargs(variants, enums),
    }
  }
}

impl Term {
  fn order_kwargs(&mut self, variants: &IndexMap<Name, Name>, enums: &IndexMap<Name, Enum>) {
    match self {
      Term::Call { fun, args, kwargs } => {
        if let Term::Var { nam } = &**fun {
          if let Some(enum_name) = variants.get(nam)
            && let Some(r#enum) = enums.get(enum_name)
          {
            let variant = r#enum.variants.get(nam).unwrap();

            let mut index_map = IndexMap::new();
            for (index, field) in variant.fields.iter().enumerate() {
              index_map.insert(field, index);
            }

            let mut kwargs = std::mem::take(kwargs);
            kwargs.sort_by_key(|i| index_map.get(&i.0).unwrap());
            let new_args = kwargs.into_iter().map(|i| i.1.clone());
            args.extend(new_args);
          }
        } else {
          fun.order_kwargs(variants, enums);
          args.iter_mut().for_each(|a| a.order_kwargs(variants, enums));
        }
      }
      Term::Lam { bod, .. } => bod.order_kwargs(variants, enums),
      Term::Bin { lhs, rhs, .. } => {
        lhs.order_kwargs(variants, enums);
        rhs.order_kwargs(variants, enums);
      }
      Term::Lst { els } | Term::Tup { els } => els.iter_mut().for_each(|e| e.order_kwargs(variants, enums)),
      Term::None | Term::Var { .. } | Term::Num { .. } | Term::Str { .. } => {}
    }
  }
}
