use crate::{
  fun::{Book, Name},
  imp::{Definition, Expr, Stmt},
};
use indexmap::IndexMap;

impl Definition {
  /// Traverses the program's definitions and adjusts the order of keyword arguments
  /// in call/constructor expressions to match the order specified in the function or constructor definition.
  pub fn order_kwargs(&mut self, book: &Book) -> Result<(), String> {
    self.body.order_kwargs(book).map_err(|e| format!("In function '{}':\n  {}", self.name, e))
  }
}

impl Stmt {
  fn order_kwargs(&mut self, book: &Book) -> Result<(), String> {
    match self {
      Stmt::Assign { val, nxt, .. } | Stmt::Ask { val, nxt, .. } => {
        val.order_kwargs(book)?;
        nxt.order_kwargs(book)?;
      }
      Stmt::InPlace { val, nxt, .. } => {
        val.order_kwargs(book)?;
        nxt.order_kwargs(book)?;
      }
      Stmt::If { cond, then, otherwise } => {
        cond.order_kwargs(book)?;
        then.order_kwargs(book)?;
        otherwise.order_kwargs(book)?;
      }
      Stmt::Match { arg, arms, .. } => {
        arg.order_kwargs(book)?;
        for arm in arms {
          arm.rgt.order_kwargs(book)?;
        }
      }
      Stmt::Switch { arg, arms, .. } => {
        arg.order_kwargs(book)?;
        for arm in arms {
          arm.order_kwargs(book)?;
        }
      }
      Stmt::Fold { arg, arms, .. } => {
        arg.order_kwargs(book)?;
        for arm in arms {
          arm.rgt.order_kwargs(book)?;
        }
      }
      Stmt::Bend { bind: _, init, cond, step, base } => {
        for init in init {
          init.order_kwargs(book)?;
        }
        cond.order_kwargs(book)?;
        step.order_kwargs(book)?;
        base.order_kwargs(book)?;
      }
      Stmt::Do { bod, .. } => bod.order_kwargs(book)?,
      Stmt::Return { term } => term.order_kwargs(book)?,
      Stmt::Err => {}
    }
    Ok(())
  }
}

impl Expr {
  fn order_kwargs(&mut self, book: &Book) -> Result<(), String> {
    match self {
      // Named arguments are only allowed when directly calling a named function.
      Expr::Call { fun, args, kwargs } => {
        if !kwargs.is_empty() {
          if let Expr::Var { nam } = fun.as_ref() {
            if let Some(names) = get_args_def_or_ctr(nam, book) {
              go_order_kwargs(&names, args, kwargs)?;
            } else {
              return Err(format!(
                "Named args are only allowed when calling a named function, not when calling variable '{nam}'."
              ));
            }
          } else {
            // TODO: Print expression
            return Err(
              "Named args are only allowed when calling a named function, not when calling an expression."
                .to_string(),
            );
          }
        }
        fun.order_kwargs(book)?;
        for arg in args {
          arg.order_kwargs(book)?;
        }
        for (_, arg) in kwargs {
          arg.order_kwargs(book)?;
        }
      }
      Expr::Lam { bod, .. } => bod.order_kwargs(book)?,
      Expr::Bin { lhs, rhs, .. } => {
        lhs.order_kwargs(book)?;
        rhs.order_kwargs(book)?;
      }
      Expr::Lst { els } | Expr::Tup { els } | Expr::Sup { els } => {
        for el in els {
          el.order_kwargs(book)?;
        }
      }
      Expr::Comprehension { term, iter, cond, .. } => {
        term.order_kwargs(book)?;
        iter.order_kwargs(book)?;
        if let Some(cond) = cond {
          cond.order_kwargs(book)?;
        }
      }
      Expr::Constructor { name, args, kwargs } => match get_args_def_or_ctr(name, book) {
        Some(names) => {
          go_order_kwargs(&names, args, kwargs)?;
          for arg in args {
            arg.order_kwargs(book)?;
          }
        }
        _ => return Err(format!("Constructor '{name}' not found.")),
      },
      Expr::MapInit { entries } => {
        for entry in entries {
          entry.1.order_kwargs(book)?;
        }
      }
      Expr::MapGet { .. } | Expr::None | Expr::Var { .. } | Expr::Num { .. } | Expr::Str { .. } => {}
    }
    Ok(())
  }
}

fn go_order_kwargs(
  names: &[Name],
  args: &mut Vec<Expr>,
  kwargs: &mut Vec<(Name, Expr)>,
) -> Result<(), String> {
  if args.len() + kwargs.len() != names.len() {
    return Err(
      "Named args are only allowed when calling a function with the exact number of arguments.".to_string(),
    );
  }
  let mut kwargs: IndexMap<Name, Expr> = IndexMap::from_iter(kwargs.drain(..));
  let remaining_names = &names[args.len() ..];
  for name in remaining_names {
    if let Some(arg) = kwargs.shift_remove(name) {
      args.push(arg);
    } else {
      return Err(format!("Named arg '{name}' is missing."));
    }
  }
  if let Some(name) = kwargs.keys().next() {
    return Err(format!("Unexpected named arg in function call {}.", name));
  }
  Ok(())
}

fn get_args_def_or_ctr(name: &Name, book: &Book) -> Option<Vec<Name>> {
  #[allow(clippy::manual_map)]
  if let Some(adt_nam) = book.ctrs.get(name) {
    Some(book.adts[adt_nam].ctrs[name].iter().map(|f| f.nam.clone()).collect())
  } else if let Some(def) = book.defs.get(name) {
    Some(def.rules[0].pats.iter().flat_map(|p| p.binds().flatten().cloned()).collect())
  } else {
    None
  }
}
