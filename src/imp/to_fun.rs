use super::{AssignPattern, Definition, Expr, Stmt};
use crate::fun::{self, Name};

impl Definition {
  pub fn to_fun(self) -> fun::Definition {
    let rule = fun::Rule {
      pats: self.params.into_iter().map(|param| fun::Pattern::Var(Some(param))).collect(),
      body: self.body.to_fun(),
    };

    fun::Definition { name: self.name, rules: vec![rule], builtin: false }
  }
}

impl AssignPattern {
  pub fn to_fun(self) -> fun::Pattern {
    match self {
      AssignPattern::Var(name) => fun::Pattern::Var(Some(name)),
      AssignPattern::Chn(name) => fun::Pattern::Chn(name),
      AssignPattern::Tup(names) => {
        fun::Pattern::Fan(fun::FanKind::Tup, fun::Tag::Static, names.into_iter().map(Self::to_fun).collect())
      }
      AssignPattern::Sup(names) => {
        fun::Pattern::Fan(fun::FanKind::Dup, fun::Tag::Auto, names.into_iter().map(Self::to_fun).collect())
      }
      AssignPattern::MapSet(..) => unreachable!(),
    }
  }
}

impl Stmt {
  pub fn to_fun(self) -> fun::Term {
    match self {
      Stmt::Assign { pat: AssignPattern::MapSet(map, key), val, nxt } => fun::Term::Let {
        pat: Box::new(fun::Pattern::Var(Some(map.clone()))),
        val: Box::new(fun::Term::call(fun::Term::Ref { nam: fun::Name::new("Map/set") }, [
          fun::Term::Var { nam: map },
          key.to_fun(),
          val.to_fun(),
        ])),
        nxt: Box::new(nxt.to_fun()),
      },
      Stmt::Assign { pat, val, nxt } => {
        let pat = pat.to_fun();
        let val = val.to_fun();
        let nxt = nxt.to_fun();
        fun::Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) }
      }
      Stmt::InPlace { op, var: nam, val, nxt } => fun::Term::Let {
        pat: Box::new(fun::Pattern::Var(Some(nam.clone()))),
        val: Box::new(fun::Term::Oper {
          opr: op.to_lang_op(),
          fst: Box::new(fun::Term::Var { nam }),
          snd: Box::new(val.to_fun()),
        }),
        nxt: Box::new(nxt.to_fun()),
      },
      Stmt::If { cond, then, otherwise } => {
        let arms = vec![otherwise.to_fun(), then.to_fun()];
        fun::Term::Swt {
          arg: Box::new(cond.to_fun()),
          bnd: Some(Name::new("%pred")),
          with: Vec::new(),
          pred: Some(Name::new("%pred-1")),
          arms,
        }
      }
      Stmt::Match { arg, bind, arms } => {
        let arg = arg.to_fun();
        let arms = arms.into_iter().map(|arm| (arm.lft, Vec::new(), arm.rgt.to_fun())).collect();
        fun::Term::Mat { arg: Box::new(arg), bnd: bind, with: Vec::new(), arms }
      }
      Stmt::Switch { arg, bind, arms } => {
        let arg = arg.to_fun();
        let pred = Some(Name::new(format!("{}-{}", bind.clone().unwrap(), arms.len() - 1)));
        let arms = arms.into_iter().map(Stmt::to_fun).collect();
        fun::Term::Swt { arg: Box::new(arg), bnd: bind, with: Vec::new(), pred, arms }
      }
      Stmt::Fold { arg, bind, arms } => {
        let arg = arg.to_fun();
        let arms = arms.into_iter().map(|arm| (arm.lft, Vec::new(), arm.rgt.to_fun())).collect();
        fun::Term::Fold { arg: Box::new(arg), bnd: bind, with: Vec::new(), arms }
      }
      Stmt::Bend { bind, init, cond, step, base } => {
        let init = init.into_iter().map(Expr::to_fun).collect();
        let cond = cond.to_fun();
        let step = step.to_fun();
        let base = base.to_fun();
        fun::Term::Bend { bind, init, cond: Box::new(cond), step: Box::new(step), base: Box::new(base) }
      }
      Stmt::Do { typ, bod } => fun::Term::Do { typ, bod: Box::new(bod.to_fun()) },
      Self::Ask { pat, val, nxt } => fun::Term::Ask {
        pat: Box::new(pat.to_fun()),
        val: Box::new(val.to_fun()),
        nxt: Box::new(nxt.to_fun()),
      },
      Stmt::Return { term } => term.to_fun(),
      Stmt::Err => fun::Term::Err,
    }
  }
}

impl Expr {
  pub fn to_fun(self) -> fun::Term {
    match self {
      Expr::None => fun::Term::Era,
      Expr::Var { nam } => fun::Term::Var { nam },
      Expr::Chn { nam } => fun::Term::Link { nam },
      Expr::Num { val } => fun::Term::Num { val: fun::Num::U24(val) },
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
      Expr::Bin { op, lhs, rhs } => {
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
      Expr::Constructor { name, args, kwargs } => {
        assert!(kwargs.is_empty());
        let args = args.into_iter().map(Self::to_fun);
        fun::Term::call(fun::Term::Ref { nam: name }, args)
      }
      Expr::Comprehension { .. } => todo!(),
      Expr::MapInit { entries } => map_init(entries),
      Expr::MapGet { .. } => unreachable!(),
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
