use indexmap::IndexMap;

use crate::fun::Name;

use super::{Definition, Enum, MBind, Program, Stmt, Term, Variant};

struct Ctx<'a> {
  variants: &'a IndexMap<Name, Name>,
  enums: &'a IndexMap<Name, Enum>,
  defs: &'a IndexMap<Name, Definition>,
}

enum Fetch<'a> {
  Variant(&'a Variant),
  Definition(&'a Definition),
}

impl<'a> Ctx<'a> {
  fn fetch(&self, name: &Name) -> Option<Fetch> {
    match self.variants.get(name) {
      Some(enum_name) => match self.enums.get(enum_name) {
        Some(r#enum) => r#enum.variants.get(name).map(Fetch::Variant),
        None => None,
      },
      None => self.defs.get(name).map(Fetch::Definition),
    }
  }
}

impl Program {
  pub fn order_kwargs(&mut self) {
    let ctx = Ctx { variants: &self.variants, enums: &self.enums, defs: &self.defs.clone() };
    for def in self.defs.values_mut() {
      def.body.order_kwargs(&ctx);
    }
  }
}

impl Stmt {
  fn order_kwargs(&mut self, ctx: &Ctx) {
    match self {
      Stmt::Assign { val, nxt, .. } => {
        val.order_kwargs(ctx);
        nxt.order_kwargs(ctx);
      }
      Stmt::InPlace { val, nxt, .. } => {
        val.order_kwargs(ctx);
        nxt.order_kwargs(ctx);
      }
      Stmt::If { cond, then, otherwise } => {
        cond.order_kwargs(ctx);
        then.order_kwargs(ctx);
        otherwise.order_kwargs(ctx);
      }
      Stmt::Match { arg, arms, .. } => {
        arg.order_kwargs(ctx);
        for arm in arms {
          arm.rgt.order_kwargs(ctx);
        }
      }
      Stmt::Switch { arg, arms, .. } => {
        arg.order_kwargs(ctx);
        for arm in arms {
          arm.order_kwargs(ctx);
        }
      }
      Stmt::Fold { arg, arms, .. } => {
        arg.order_kwargs(ctx);
        for arm in arms {
          arm.rgt.order_kwargs(ctx);
        }
      }
      Stmt::Do { block, .. } => {
        for bind in block {
          match bind {
            MBind::Ask { val, .. } => val.order_kwargs(ctx),
            MBind::Stmt { stmt } => stmt.order_kwargs(ctx),
          }
        }
      }
      Stmt::Return { term } => term.order_kwargs(ctx),
    }
  }
}

impl Term {
  fn order_kwargs(&mut self, ctx: &Ctx) {
    match self {
      Term::Call { fun, args, kwargs } => {
        if let Term::Var { nam } = &**fun {
          if let Some(fetch) = ctx.fetch(nam) {
            match fetch {
              Fetch::Variant(variant) => go_order_kwargs(&variant.fields, kwargs, args),
              Fetch::Definition(def) => go_order_kwargs(&def.params, kwargs, args),
            }
          }
        } else {
          fun.order_kwargs(ctx);
          args.iter_mut().for_each(|a| a.order_kwargs(ctx));
        }
      }
      Term::Lam { bod, .. } => bod.order_kwargs(ctx),
      Term::Bin { lhs, rhs, .. } => {
        lhs.order_kwargs(ctx);
        rhs.order_kwargs(ctx);
      }
      Term::Lst { els } | Term::Tup { els } => els.iter_mut().for_each(|e| e.order_kwargs(ctx)),
      Term::Comprehension { .. } => {}
      Term::None | Term::Var { .. } | Term::Num { .. } | Term::Str { .. } => {}
    }
  }
}

fn go_order_kwargs(names: &[Name], kwargs: &mut Vec<(Name, Term)>, args: &mut Vec<Term>) {
  let mut index_map = IndexMap::new();
  for (index, field) in names.iter().enumerate() {
    index_map.insert(field, index);
  }

  let mut kwargs = std::mem::take(kwargs);
  kwargs.sort_by_key(|i| index_map.get(&i.0).unwrap());
  let new_args = kwargs.into_iter().map(|i| i.1.clone());
  args.extend(new_args);
}
