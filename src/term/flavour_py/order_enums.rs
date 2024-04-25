use indexmap::IndexMap;

use crate::term::Name;

use super::{Enum, Program, Stmt, Term};

impl Program {
  pub fn order_enums(&mut self) {
    for def in self.defs.values_mut() {
      def.body.order_enums(&self.variants, &self.enums);
    }
  }
}

impl Stmt {
  fn order_enums(&mut self, variants: &IndexMap<Name, Name>, enums: &IndexMap<Name, Enum>) {
    match self {
      Stmt::Assign { val, nxt, .. } => {
        val.order_enums(variants, enums);
        nxt.order_enums(variants, enums);
      }
      Stmt::If { cond, then, otherwise } => {
        cond.order_enums(variants, enums);
        then.order_enums(variants, enums);
        otherwise.order_enums(variants, enums);
      }
      Stmt::Match { arg, arms, .. } => {
        arg.order_enums(variants, enums);
        for arm in arms {
          arm.rgt.order_enums(variants, enums);
        }
      }
      Stmt::Return { term } => term.order_enums(variants, enums),
    }
  }
}

impl Term {
  fn order_enums(&mut self, variants: &IndexMap<Name, Name>, enums: &IndexMap<Name, Enum>) {
    match self {
      Term::Enum { nam, fields } => {
        if let Some(enum_name) = variants.get(nam)
          && let Some(r#enum) = enums.get(enum_name)
        {
          let variant = r#enum.variants.get(nam).unwrap();

          let mut index_map = IndexMap::new();
          for (index, field_name) in variant.fields.iter().enumerate() {
            index_map.insert(field_name, index);
          }

          fields.sort_by_key(|i| index_map.get(&i.0).unwrap());
        }
      }
      Term::Lam { bod, .. } => bod.order_enums(variants, enums),
      Term::Call { fun, args } => {
        fun.order_enums(variants, enums);
        args.iter_mut().for_each(|a| a.order_enums(variants, enums));
      }
      Term::Bin { lhs, rhs, .. } => {
        lhs.order_enums(variants, enums);
        rhs.order_enums(variants, enums);
      }
      Term::Lst { els } | Term::Tup { els } => els.iter_mut().for_each(|e| e.order_enums(variants, enums)),
      Term::None | Term::Var { .. } | Term::Num { .. } | Term::Str { .. } => {}
    }
  }
}
