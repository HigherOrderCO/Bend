use super::{AssignPattern, Definition, Expr, InPlaceOp, Stmt};
use crate::{
  diagnostics::Diagnostics,
  fun::{
    self,
    builtins::{LCONS, LNIL},
    parser::ParseBook,
    Book, Name,
  },
};

impl ParseBook {
  // TODO: Change all functions to return diagnostics
  pub fn to_fun(mut self) -> Result<Book, Diagnostics> {
    for (name, mut def) in std::mem::take(&mut self.imp_defs) {
      def.order_kwargs(&self)?;
      def.gen_map_get();

      if self.fun_defs.contains_key(&name) {
        panic!("Def names collision should be checked at parse time")
      }

      self.fun_defs.insert(name, def.to_fun()?);
    }

    let ParseBook { fun_defs: defs, hvm_defs, adts, ctrs, import_ctx, .. } = self;
    Ok(Book { defs, hvm_defs, adts, ctrs, entrypoint: None, imports: import_ctx.to_imports() })
  }
}

impl Definition {
  pub fn to_fun(self) -> Result<fun::Definition, String> {
    let body = self.body.into_fun().map_err(|e| format!("In function '{}': {}", self.name, e))?;
    let body = match body {
      StmtToFun::Return(term) => term,
      StmtToFun::Assign(..) => {
        return Err(format!("Function '{}' doesn't end with a return statement", self.name));
      }
    };

    let rule =
      fun::Rule { pats: self.params.into_iter().map(|param| fun::Pattern::Var(Some(param))).collect(), body };

    let def = fun::Definition::new(self.name, vec![rule], self.source);
    Ok(def)
  }
}

impl AssignPattern {
  pub fn into_fun(self) -> fun::Pattern {
    match self {
      AssignPattern::Eraser => fun::Pattern::Var(None),
      AssignPattern::Var(name) => fun::Pattern::Var(Some(name)),
      AssignPattern::Chn(name) => fun::Pattern::Chn(name),
      AssignPattern::Tup(names) => fun::Pattern::Fan(
        fun::FanKind::Tup,
        fun::Tag::Static,
        names.into_iter().map(Self::into_fun).collect(),
      ),
      AssignPattern::Sup(names) => {
        fun::Pattern::Fan(fun::FanKind::Dup, fun::Tag::Auto, names.into_iter().map(Self::into_fun).collect())
      }
      AssignPattern::MapSet(..) => unreachable!(),
    }
  }
}

#[derive(Debug)]
enum StmtToFun {
  Return(fun::Term),
  Assign(bool, fun::Pattern, fun::Term),
}

fn take(t: Stmt) -> Result<(bool, Option<fun::Pattern>, fun::Term), String> {
  match t.into_fun()? {
    StmtToFun::Return(ret) => Ok((false, None, ret)),
    StmtToFun::Assign(x, pat, val) => Ok((x, Some(pat), val)),
  }
}

fn wrap(nxt: Option<fun::Pattern>, term: fun::Term, ask: bool) -> StmtToFun {
  if let Some(pat) = nxt {
    StmtToFun::Assign(ask, pat, term)
  } else {
    StmtToFun::Return(term)
  }
}

impl Stmt {
  fn into_fun(self) -> Result<StmtToFun, String> {
    // TODO: Refactor this to not repeat everything.
    // TODO: When we have an error with an assignment, we should show the offending assignment (eg. "{pat} = ...").
    let stmt_to_fun = match self {
      Stmt::Assign { pat: AssignPattern::MapSet(map, key), val, nxt: Some(nxt) } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let term = fun::Term::Let {
          pat: Box::new(fun::Pattern::Var(Some(map.clone()))),
          val: Box::new(fun::Term::call(
            fun::Term::Ref { nam: fun::Name::new("Map/set") },
            [fun::Term::Var { nam: map }, key.to_fun(), val.to_fun()],
          )),
          nxt: Box::new(nxt),
        };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Assign { pat: AssignPattern::MapSet(..), val: _, nxt: None } => {
        return Err("Branch ends with map assignment.".to_string());
      }
      Stmt::Assign { pat, val, nxt: Some(nxt) } => {
        let pat = pat.into_fun();
        let val = val.to_fun();
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let term = fun::Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Assign { pat, val, nxt: None } => {
        let pat = pat.into_fun();
        let val = val.to_fun();
        StmtToFun::Assign(false, pat, val)
      }
      Stmt::InPlace { op, pat, val, nxt } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        // if it is a mapper operation
        if let InPlaceOp::Map = op {
          let term = match &*pat {
            AssignPattern::MapSet(map, key) => {
              let rhs = fun::Term::call(
                fun::Term::r#ref("Map/map"),
                [fun::Term::Var { nam: map.clone() }, key.clone().to_fun(), val.clone().to_fun()],
              );
              fun::Term::Let {
                pat: Box::new(fun::Pattern::Var(Some(map.clone()))),
                val: Box::new(rhs),
                nxt: Box::new(nxt),
              }
            }
            _ => {
              let rhs = fun::Term::call(val.to_fun(), [pat.clone().into_fun().to_term()]);
              fun::Term::Let { pat: Box::new(pat.into_fun()), val: Box::new(rhs), nxt: Box::new(nxt) }
            }
          };

          return Ok(wrap(nxt_pat, term, ask));
        }

        // otherwise
        match *pat {
          AssignPattern::Var(var) => {
            let term = fun::Term::Let {
              pat: Box::new(fun::Pattern::Var(Some(var.clone()))),
              val: Box::new(fun::Term::Oper {
                opr: op.to_lang_op(),
                fst: Box::new(fun::Term::Var { nam: var }),
                snd: Box::new(val.to_fun()),
              }),
              nxt: Box::new(nxt),
            };
            wrap(nxt_pat, term, ask)
          }
          AssignPattern::MapSet(map, key) => {
            let temp = Name::new("%0");
            let partial =
              Expr::Opr { op: op.to_lang_op(), lhs: Box::new(Expr::Var { nam: temp.clone() }), rhs: val };
            let map_fn = Expr::Lam { names: vec![(temp, false)], bod: Box::new(partial) };
            let map_term = fun::Term::call(
              fun::Term::r#ref("Map/map"),
              [fun::Term::Var { nam: map.clone() }, key.to_fun(), map_fn.to_fun()],
            );
            let term = fun::Term::Let {
              pat: Box::new(fun::Pattern::Var(Some(map))),
              val: Box::new(map_term),
              nxt: Box::new(nxt),
            };
            wrap(nxt_pat, term, ask)
          }
          _ => unreachable!(),
        }
      }
      Stmt::If { cond, then, otherwise, nxt } => {
        let (ask, pat, then, else_) = match (then.into_fun()?, otherwise.into_fun()?) {
          (StmtToFun::Return(t), StmtToFun::Return(e)) => (false, None, t, e),
          (StmtToFun::Assign(ask, tp, t), StmtToFun::Assign(ask_, ep, e)) if tp == ep => {
            (ask && ask_, Some(tp), t, e)
          }
          (StmtToFun::Assign(..), StmtToFun::Assign(..)) => {
            return Err("'if' branches end with different assignments.".to_string());
          }
          (StmtToFun::Return(..), StmtToFun::Assign(..)) => {
            return Err(
              "Expected 'else' branch from 'if' to return, but it ends with assignment.".to_string(),
            );
          }
          (StmtToFun::Assign(..), StmtToFun::Return(..)) => {
            return Err(
              "Expected 'else' branch from 'if' to end with assignment, but it returns.".to_string(),
            );
          }
        };
        let arms = vec![else_, then];
        let term = fun::Term::Swt {
          arg: Box::new(cond.to_fun()),
          bnd: Some(Name::new("%pred")),
          with_bnd: vec![],
          with_arg: vec![],
          pred: Some(Name::new("%pred-1")),
          arms,
        };
        wrap_nxt_assign_stmt(term, nxt, pat, ask)?
      }
      Stmt::Match { arg, bnd, with_bnd, with_arg, arms, nxt } => {
        let arg = arg.to_fun();
        let mut fun_arms = vec![];
        let mut arms = arms.into_iter();
        let fst = arms.next().unwrap();
        let (fst_ask, fst_pat, fst_rgt) = take(fst.rgt)?;
        let with_arg = with_arg.into_iter().map(Expr::to_fun).collect();
        fun_arms.push((fst.lft, vec![], fst_rgt));
        for arm in arms {
          let (arm_ask, arm_pat, arm_rgt) = take(arm.rgt)?;
          match (&arm_pat, &fst_pat) {
            (Some(arm_pat), Some(fst_pat)) if arm_pat != fst_pat || arm_ask != fst_ask => {
              return Err("'match' arms end with different assignments.".to_string());
            }
            (Some(_), None) => {
              return Err("Expected 'match' arms to end with assignment, but it returns.".to_string());
            }
            (None, Some(_)) => {
              return Err("Expected 'match' arms to return, but it ends with assignment.".to_string());
            }
            (Some(_), Some(_)) => fun_arms.push((arm.lft, vec![], arm_rgt)),
            (None, None) => fun_arms.push((arm.lft, vec![], arm_rgt)),
          }
        }
        let term = fun::Term::Mat { arg: Box::new(arg), bnd, with_bnd, with_arg, arms: fun_arms };
        wrap_nxt_assign_stmt(term, nxt, fst_pat, fst_ask)?
      }
      Stmt::Switch { arg, bnd, with_bnd, with_arg, arms, nxt } => {
        let arg = arg.to_fun();
        let mut fun_arms = vec![];
        let mut arms = arms.into_iter();
        let fst = arms.next().unwrap();
        let (fst_ask, fst_pat, fst) = take(fst)?;
        let with_arg = with_arg.into_iter().map(Expr::to_fun).collect();
        fun_arms.push(fst);
        for arm in arms {
          let (arm_ask, arm_pat, arm) = take(arm)?;
          match (&arm_pat, &fst_pat) {
            (Some(arm_pat), Some(fst_pat)) if arm_pat != fst_pat || arm_ask != fst_ask => {
              return Err("'switch' arms end with different assignments.".to_string());
            }
            (Some(_), None) => {
              return Err("Expected 'switch' arms to end with assignment, but it returns.".to_string());
            }
            (None, Some(_)) => {
              return Err("Expected 'switch' arms to return, but it ends with assignment.".to_string());
            }
            (Some(_), Some(_)) => fun_arms.push(arm),
            (None, None) => fun_arms.push(arm),
          }
        }
        let pred = Some(Name::new(format!("{}-{}", bnd.clone().unwrap(), fun_arms.len() - 1)));
        let term = fun::Term::Swt { arg: Box::new(arg), bnd, with_bnd, with_arg, pred, arms: fun_arms };
        wrap_nxt_assign_stmt(term, nxt, fst_pat, fst_ask)?
      }
      Stmt::Fold { arg, bnd, with_bnd, with_arg, arms, nxt } => {
        let arg = arg.to_fun();
        let mut fun_arms = vec![];
        let mut arms = arms.into_iter();
        let fst = arms.next().unwrap();
        let (fst_ask, fst_pat, fst_rgt) = take(fst.rgt)?;
        fun_arms.push((fst.lft, vec![], fst_rgt));
        let with_arg = with_arg.into_iter().map(Expr::to_fun).collect();
        for arm in arms {
          let (arm_ask, arm_pat, arm_rgt) = take(arm.rgt)?;
          match (&arm_pat, &fst_pat) {
            (Some(arm_pat), Some(fst_pat)) if arm_pat != fst_pat || arm_ask != fst_ask => {
              return Err("'fold' arms end with different assignments.".to_string());
            }
            (Some(_), None) => {
              return Err("Expected 'fold' arms to end with assignment, but it returns.".to_string());
            }
            (None, Some(_)) => {
              return Err("Expected 'fold' arms to return, but it ends with assignment.".to_string());
            }
            (Some(_), Some(_)) => fun_arms.push((arm.lft, vec![], arm_rgt)),
            (None, None) => fun_arms.push((arm.lft, vec![], arm_rgt)),
          }
        }
        let term = fun::Term::Fold { arg: Box::new(arg), bnd, with_bnd, with_arg, arms: fun_arms };
        wrap_nxt_assign_stmt(term, nxt, fst_pat, fst_ask)?
      }
      Stmt::Bend { bnd, arg, cond, step, base, nxt } => {
        let arg = arg.into_iter().map(Expr::to_fun).collect();
        let cond = cond.to_fun();
        let (ask, pat, step, base) = match (step.into_fun()?, base.into_fun()?) {
          (StmtToFun::Return(s), StmtToFun::Return(b)) => (false, None, s, b),
          (StmtToFun::Assign(aa, sp, s), StmtToFun::Assign(ba, bp, b)) if sp == bp => {
            (aa && ba, Some(sp), s, b)
          }
          (StmtToFun::Assign(..), StmtToFun::Assign(..)) => {
            return Err("'bend' branches end with different assignments.".to_string());
          }
          (StmtToFun::Return(..), StmtToFun::Assign(..)) => {
            return Err(
              "Expected 'else' branch from 'bend' to return, but it ends with assignment.".to_string(),
            );
          }
          (StmtToFun::Assign(..), StmtToFun::Return(..)) => {
            return Err(
              "Expected 'else' branch from 'bend' to end with assignment, but it returns.".to_string(),
            );
          }
        };
        let term =
          fun::Term::Bend { bnd, arg, cond: Box::new(cond), step: Box::new(step), base: Box::new(base) };
        wrap_nxt_assign_stmt(term, nxt, pat, ask)?
      }
      Stmt::With { typ, bod, nxt } => {
        let (ask, pat, bod) = take(*bod)?;
        let term = fun::Term::With { typ, bod: Box::new(bod) };
        wrap_nxt_assign_stmt(term, nxt, pat, ask)?
      }
      Stmt::Ask { pat, val, nxt: Some(nxt) } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let term =
          fun::Term::Ask { pat: Box::new(pat.into_fun()), val: Box::new(val.to_fun()), nxt: Box::new(nxt) };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Ask { pat, val, nxt: None } => {
        let pat = pat.into_fun();
        let val = val.to_fun();
        StmtToFun::Assign(true, pat, val)
      }
      Stmt::Open { typ, var, nxt } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let term = fun::Term::Open { typ, var, bod: Box::new(nxt) };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Use { nam, val, nxt } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let term = fun::Term::Use { nam: Some(nam), val: Box::new(val.to_fun()), nxt: Box::new(nxt) };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Return { term } => StmtToFun::Return(term.to_fun()),
      Stmt::LocalDef { def, nxt } => {
        let (ask, nxt_pat, nxt) = take(*nxt)?;
        let def = def.to_fun()?;
        let term = fun::Term::Def { def, nxt: Box::new(nxt) };
        wrap(nxt_pat, term, ask)
      }
      Stmt::Err => unreachable!(),
    };
    Ok(stmt_to_fun)
  }
}

impl Expr {
  pub fn to_fun(self) -> fun::Term {
    match self {
      Expr::Era => fun::Term::Era,
      Expr::Var { nam } => fun::Term::Var { nam },
      Expr::Chn { nam } => fun::Term::Link { nam },
      Expr::Num { val } => fun::Term::Num { val },
      Expr::Call { fun, args, kwargs } => {
        assert!(kwargs.is_empty());
        let args = args.into_iter().map(Self::to_fun);
        fun::Term::call(fun.to_fun(), args)
      }
      Expr::Lam { names, bod } => names.into_iter().rfold(bod.to_fun(), |acc, (name, link)| fun::Term::Lam {
        tag: fun::Tag::Static,
        pat: Box::new(if link { fun::Pattern::Chn(name) } else { fun::Pattern::Var(Some(name)) }),
        bod: Box::new(acc),
      }),
      Expr::Opr { op, lhs, rhs } => {
        fun::Term::Oper { opr: op, fst: Box::new(lhs.to_fun()), snd: Box::new(rhs.to_fun()) }
      }
      Expr::Str { val } => fun::Term::Str { val },
      Expr::Lst { els } => fun::Term::List { els: els.into_iter().map(Self::to_fun).collect() },
      Expr::Tup { els } => fun::Term::Fan {
        fan: fun::FanKind::Tup,
        tag: fun::Tag::Static,
        els: els.into_iter().map(Self::to_fun).collect(),
      },
      Expr::Sup { els } => fun::Term::Fan {
        fan: fun::FanKind::Dup,
        tag: fun::Tag::Auto,
        els: els.into_iter().map(Self::to_fun).collect(),
      },
      Expr::Ctr { name, args, kwargs } => {
        assert!(kwargs.is_empty());
        let args = args.into_iter().map(Self::to_fun);
        fun::Term::call(fun::Term::Var { nam: name }, args)
      }
      Expr::LstMap { term, bind, iter, cond } => {
        const ITER_TAIL: &str = "%iter.tail";
        const ITER_HEAD: &str = "%iter.head";

        let cons_branch = fun::Term::call(
          fun::Term::r#ref(LCONS),
          [term.to_fun(), fun::Term::Var { nam: Name::new(ITER_TAIL) }],
        );
        let cons_branch = if let Some(cond) = cond {
          fun::Term::Swt {
            arg: Box::new(cond.to_fun()),
            bnd: Some(Name::new("%comprehension")),
            with_bnd: vec![],
            with_arg: vec![],
            pred: Some(Name::new("%comprehension-1")),
            arms: vec![fun::Term::Var { nam: Name::new(ITER_TAIL) }, cons_branch],
          }
        } else {
          cons_branch
        };
        let cons_branch = fun::Term::Let {
          pat: Box::new(fun::Pattern::Var(Some(bind))),
          val: Box::new(fun::Term::Var { nam: Name::new(ITER_HEAD) }),
          nxt: Box::new(cons_branch),
        };

        fun::Term::Fold {
          bnd: Some(Name::new("%iter")),
          arg: Box::new(iter.to_fun()),
          with_bnd: vec![],
          with_arg: vec![],
          arms: vec![
            (Some(Name::new(LNIL)), vec![], fun::Term::r#ref(LNIL)),
            (Some(Name::new(LCONS)), vec![], cons_branch),
          ],
        }
      }
      Expr::Map { entries } => map_init(entries),
      Expr::MapGet { .. } => unreachable!(),
      Expr::TreeNode { left, right } => {
        let left = left.to_fun();
        let right = right.to_fun();
        fun::Term::call(fun::Term::r#ref("Tree/Node"), [left, right])
      }
      Expr::TreeLeaf { val } => {
        let val = val.to_fun();
        fun::Term::app(fun::Term::r#ref("Tree/Leaf"), val)
      }
    }
  }
}

fn map_init(entries: Vec<(Expr, Expr)>) -> fun::Term {
  let mut map = fun::Term::Ref { nam: fun::Name::new("Map/empty") };
  for (key, value) in entries {
    map =
      fun::Term::call(fun::Term::Ref { nam: fun::Name::new("Map/set") }, [map, key.to_fun(), value.to_fun()]);
  }
  map
}

/// If the statement was a return, returns it, erroring if there is another after it.
/// Otherwise, turns it into a 'let' and returns the next statement.
fn wrap_nxt_assign_stmt(
  term: fun::Term,
  nxt: Option<Box<Stmt>>,
  pat: Option<fun::Pattern>,
  ask: bool,
) -> Result<StmtToFun, String> {
  if let Some(nxt) = nxt {
    if let Some(pat) = pat {
      let (ask_nxt, nxt_pat, nxt) = take(*nxt)?;
      let term = if ask {
        fun::Term::Ask { pat: Box::new(pat), val: Box::new(term), nxt: Box::new(nxt) }
      } else {
        fun::Term::Let { pat: Box::new(pat), val: Box::new(term), nxt: Box::new(nxt) }
      };
      Ok(wrap(nxt_pat, term, ask_nxt))
    } else {
      Err("Statement ends with return but is not at end of function.".to_string())
    }
  } else if let Some(pat) = pat {
    Ok(StmtToFun::Assign(ask, pat, term))
  } else {
    Ok(StmtToFun::Return(term))
  }
}
