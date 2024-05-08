use indexmap::IndexMap;

use crate::fun::Name;

use super::{Definition, Enum, Expr, MBind, Program, Stmt, Variant};

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
      Stmt::Bend { bind: _, init, cond, step, base } => {
        for init in init {
          init.order_kwargs(ctx);
        }
        cond.order_kwargs(ctx);
        step.order_kwargs(ctx);
        base.order_kwargs(ctx);
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

impl Expr {
  fn order_kwargs(&mut self, ctx: &Ctx) {
    match self {
      Expr::Call { fun, args, kwargs } => {
        if let Expr::Var { nam } = &**fun {
          if let Some(fetch) = ctx.fetch(nam) {
            match fetch {
              Fetch::Variant(variant) => go_order_kwargs(variant.fields.iter().map(|f| &f.nam), kwargs, args),
              Fetch::Definition(def) => go_order_kwargs(def.params.iter(), kwargs, args),
            }
          }
        } else {
          fun.order_kwargs(ctx);
          args.iter_mut().for_each(|a| a.order_kwargs(ctx));
        }
      }
      Expr::Lam { bod, .. } => bod.order_kwargs(ctx),
      Expr::Bin { lhs, rhs, .. } => {
        lhs.order_kwargs(ctx);
        rhs.order_kwargs(ctx);
      }
      Expr::Lst { els } | Expr::Tup { els } => els.iter_mut().for_each(|e| e.order_kwargs(ctx)),
      Expr::Comprehension { term, iter, cond, .. } => {
        term.order_kwargs(ctx);
        iter.order_kwargs(ctx);
        if let Some(cond) = cond {
          cond.order_kwargs(ctx);
        }
      }
      Expr::MapInit { entries } => {
        for entry in entries {
          entry.1.order_kwargs(ctx);
        }
      }
      Expr::MapGet { .. } | Expr::None | Expr::Var { .. } | Expr::Num { .. } | Expr::Str { .. } => {}
    }
  }
}

fn go_order_kwargs<'a>(
  names: impl Iterator<Item = &'a Name>,
  kwargs: &mut Vec<(Name, Expr)>,
  args: &mut Vec<Expr>,
) {
  let mut index_map = IndexMap::new();
  for (index, field) in names.enumerate() {
    index_map.insert(field, index);
  }

  let mut kwargs = std::mem::take(kwargs);
  kwargs.sort_by_key(|i| index_map.get(&i.0).unwrap());
  let new_args = kwargs.into_iter().map(|i| i.1.clone());
  args.extend(new_args);
}
