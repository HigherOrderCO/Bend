use crate::term as lang;

use super::{AssignPattern, Definition, Program, Stmt, Term};

impl Program {
  pub fn to_lang(self, mut book: lang::Book) -> lang::Book {
    for (def_name, def) in self.defs {
      book.defs.insert(def_name, def.to_lang());
    }

    for (enum_name, r#enum) in self.enums {
      for variant in r#enum.variants.iter() {
        book.ctrs.insert(variant.0.clone(), enum_name.clone());
      }
      let ctrs = r#enum.variants.into_iter().map(|(k, v)| (k, v.fields)).collect();
      let adt = lang::Adt { ctrs, builtin: false };
      book.adts.insert(enum_name, adt);
    }

    book
  }
}

impl Definition {
  pub fn to_lang(self) -> lang::Definition {
    let rule = lang::Rule {
      pats: self.params.into_iter().map(|param| lang::Pattern::Var(Some(param))).collect(),
      body: self.body.to_lang(),
    };

    lang::Definition { name: self.name, rules: vec![rule], builtin: false }
  }
}

impl AssignPattern {
  pub fn to_lang(self) -> lang::Pattern {
    match self {
      AssignPattern::Var(name) => lang::Pattern::Var(Some(name)),
      AssignPattern::Tup(names) => lang::Pattern::Fan(
        lang::FanKind::Tup,
        lang::Tag::Static,
        names.into_iter().map(|name| lang::Pattern::Var(Some(name))).collect(),
      ),
    }
  }
}

impl Stmt {
  pub fn to_lang(self) -> lang::Term {
    match self {
      Stmt::Assign { pat, val, nxt } => {
        let pat = pat.to_lang();
        let val = val.to_lang();
        let nxt = nxt.to_lang();
        lang::Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) }
      }
      Stmt::If { cond, then, otherwise } => {
        let arms = vec![otherwise.to_lang(), then.to_lang()];
        lang::Term::Swt { arg: Box::new(cond.to_lang()), bnd: None, with: Vec::new(), pred: None, arms }
      }
      Stmt::Match { arg, bind, arms: old_arms } => {
        let arg = arg.to_lang();
        let mut arms = Vec::with_capacity(old_arms.len());
        for arm in old_arms {
          arms.push((arm.lft, Vec::new(), arm.rgt.to_lang()))
        }
        lang::Term::Mat { arg: Box::new(arg), bnd: bind, with: Vec::new(), arms }
      }
      Stmt::Return { term } => term.to_lang(),
    }
  }
}

impl Term {
  pub fn to_lang(self) -> lang::Term {
    match self {
      Term::None => lang::Term::Era,
      Term::Var { nam } => lang::Term::Var { nam },
      Term::Num { val } => lang::Term::Num { typ: lang::NumType::U24, val },
      Term::Call { fun, args } => {
        let args = args.into_iter().map(Self::to_lang);
        lang::Term::call(fun.to_lang(), args)
      }
      Term::Lam { pat, bod } => {
        lang::Term::Lam { tag: lang::Tag::Static, pat: Box::new(pat.to_lang()), bod: Box::new(bod.to_lang()) }
      }
      Term::Enum { nam, fields } => {
        let fun = lang::Term::Var { nam };
        let args = fields.into_iter().map(|(_, arg)| arg.to_lang()).collect::<Vec<_>>();
        lang::Term::call(fun, args)
      }
      Term::Bin { op, lhs, rhs } => {
        lang::Term::Opr { opr: op, fst: Box::new(lhs.to_lang()), snd: Box::new(rhs.to_lang()) }
      }
      Term::Str { val } => lang::Term::Str { val },
      Term::Lst { els } => lang::Term::Lst { els: els.into_iter().map(Self::to_lang).collect() },
      Term::Tup { els } => lang::Term::Fan {
        fan: lang::FanKind::Tup,
        tag: lang::Tag::Static,
        els: els.into_iter().map(Self::to_lang).collect(),
      },
    }
  }
}
