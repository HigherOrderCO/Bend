use crate::term::{self, Name};

use super::{AssignPattern, Definition, Program, Stmt, Term};

impl Program {
  pub fn to_lang(self, mut book: term::Book) -> term::Book {
    for (def_name, def) in self.defs {
      book.defs.insert(def_name, def.to_lang());
    }

    for (enum_name, r#enum) in self.enums {
      for variant in r#enum.variants.iter() {
        book.ctrs.insert(variant.0.clone(), enum_name.clone());
      }
      let ctrs = r#enum.variants.into_iter().map(|(k, v)| (k, v.fields)).collect();
      let adt = term::Adt { ctrs, builtin: false };
      book.adts.insert(enum_name, adt);
    }

    book
  }
}

impl Definition {
  pub fn to_lang(self) -> term::Definition {
    let rule = term::Rule {
      pats: self.params.into_iter().map(|param| term::Pattern::Var(Some(param))).collect(),
      body: self.body.to_lang(),
    };

    term::Definition { name: self.name, rules: vec![rule], builtin: false }
  }
}

impl Stmt {
  pub fn to_lang(self) -> term::Term {
    match self {
      Stmt::Assign { pat, val, nxt } => {
        let pat = match pat {
          AssignPattern::Var(name) => term::Pattern::Var(Some(name)),
          AssignPattern::Tup(names) => term::Pattern::Fan(
            term::FanKind::Dup,
            term::Tag::Static,
            names.into_iter().map(|name| term::Pattern::Var(Some(name))).collect(),
          ),
        };
        let val = val.to_lang();
        let nxt = nxt.to_lang();
        term::Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) }
      }
      Stmt::If { cond, then, otherwise } => {
        let arms = vec![
          (Some(Name::new("True")), vec![], then.to_lang()),
          (Some(Name::new("False")), vec![], otherwise.to_lang()),
        ];
        term::Term::Mat { arg: Box::new(cond.to_lang()), bnd: None, with: Vec::new(), arms }
      }
      Stmt::Match { arg, bind, arms: old_arms } => {
        let arg = arg.to_lang();
        let mut arms = Vec::with_capacity(old_arms.len());
        for arm in old_arms {
          arms.push((arm.lft, Vec::new(), arm.rgt.to_lang()))
        }
        term::Term::Mat { arg: Box::new(arg), bnd: bind, with: Vec::new(), arms }
      }
      Stmt::Return { term } => term.to_lang(),
    }
  }
}

impl Term {
  pub fn to_lang(self) -> term::Term {
    match self {
      Term::None => term::Term::Era,
      Term::Var { nam } => term::Term::Var { nam },
      Term::Num { val } => term::Term::Num { typ: term::NumType::U24, val },
      Term::Call { fun, args } => {
        let args = args.into_iter().map(Self::to_lang);
        term::Term::call(fun.to_lang(), args)
      }
      Term::Lam { pat, bod } => {
        term::Term::Lam { tag: term::Tag::Static, pat: Box::new(pat), bod: Box::new(bod.to_lang()) }
      }
      Term::Enum { nam, fields } => {
        let fun = term::Term::Var { nam };
        let args = fields.into_iter().map(|(_, arg)| arg.to_lang()).collect::<Vec<_>>();
        term::Term::call(fun, args)
      }
      Term::Bin { op, lhs, rhs } => {
        term::Term::Opr { opr: op, fst: Box::new(lhs.to_lang()), snd: Box::new(rhs.to_lang()) }
      }
      Term::Str { val } => term::Term::Str { val },
      Term::Lst { els } => term::Term::Lst { els: els.into_iter().map(Self::to_lang).collect() },
      Term::Tup { els } => term::Term::Fan {
        fan: term::FanKind::Tup,
        tag: term::Tag::Static,
        els: els.into_iter().map(Self::to_lang).collect(),
      },
    }
  }
}
