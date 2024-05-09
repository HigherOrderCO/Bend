use super::{AssignPattern, Definition, Expr, Program, Stmt};
use crate::fun;

impl Program {
  pub fn to_fun(self, mut book: fun::Book) -> fun::Book {
    for (def_name, def) in self.defs {
      book.defs.insert(def_name, def.to_fun());
    }

    for (enum_name, r#enum) in self.enums {
      for variant in r#enum.variants.iter() {
        book.ctrs.insert(variant.0.clone(), enum_name.clone());
      }
      let ctrs = r#enum.variants.into_iter().map(|(k, v)| (k, v.fields)).collect();
      let adt = fun::Adt { ctrs, builtin: false };
      book.adts.insert(enum_name, adt);
    }

    book
  }
}

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
      AssignPattern::Tup(names) => fun::Pattern::Fan(
        fun::FanKind::Tup,
        fun::Tag::Static,
        names.into_iter().map(|name| fun::Pattern::Var(Some(name))).collect(),
      ),
    }
  }
}

impl Stmt {
  pub fn to_fun(self) -> fun::Term {
    match self {
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
        fun::Term::Swt { arg: Box::new(cond.to_fun()), bnd: None, with: Vec::new(), pred: None, arms }
      }
      Stmt::Match { arg, bind, arms } => {
        let arg = arg.to_fun();
        let arms = arms.into_iter().map(|arm| (arm.lft, Vec::new(), arm.rgt.to_fun())).collect();
        fun::Term::Mat { arg: Box::new(arg), bnd: bind, with: Vec::new(), arms }
      }
      Stmt::Switch { arg, bind, arms } => {
        let arg = arg.to_fun();
        let arms = arms.into_iter().map(Stmt::to_fun).collect();
        fun::Term::Swt { arg: Box::new(arg), bnd: bind, with: Vec::new(), pred: None, arms }
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
      Stmt::Do { .. } => todo!(),
      Stmt::Return { term } => term.to_fun(),
    }
  }
}

impl Expr {
  pub fn to_fun(self) -> fun::Term {
    match self {
      Expr::None => fun::Term::Era,
      Expr::Var { nam } => fun::Term::Var { nam },
      Expr::Num { val } => fun::Term::Num { val: fun::Num::U24(val) },
      Expr::Call { fun, args, kwargs } => {
        assert!(kwargs.is_empty());
        let args = args.into_iter().map(Self::to_fun);
        fun::Term::call(fun.to_fun(), args)
      }
      Expr::Lam { names, bod } => names.into_iter().rfold(bod.to_fun(), |acc, nxt| fun::Term::Lam {
        tag: fun::Tag::Static,
        pat: Box::new(fun::Pattern::Var(Some(nxt))),
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
      Expr::Comprehension { .. } => todo!(),
    }
  }
}
