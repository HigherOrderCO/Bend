use super::{DefId, Name, Number};
use itertools::Itertools;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, Default)]
pub struct DefinitionBook {
  pub defs: HashMap<DefId, Definition>,
}

#[derive(Debug, Clone)]
pub struct Definition {
  pub name: Name,
  pub rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
pub struct Rule {
  pub def_id: DefId,
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub enum Pattern {
  Ctr(Name, Vec<Pattern>),
  Num(Number),
  Var(Option<Name>),
}

#[derive(Debug, Clone)]
pub enum Type {
  Any,
  Number,
  Adt(HashMap<Name, Vec<Type>>),
}

#[derive(Debug, Clone)]
pub enum Term {
  Lam { nam: Option<Name>, bod: Box<Term> },
  Var { nam: Name },
  GlobalLam { nam: Name, bod: Box<Term> },
  GlobalVar { nam: Name },
  Ref { def_id: DefId },
  App { fun: Box<Term>, arg: Box<Term> },
  Dup { fst: Option<Name>, snd: Option<Name>, val: Box<Term>, nxt: Box<Term> },
  Num { val: Number },
  NumOp { op: NumOper, fst: Box<Term>, snd: Box<Term> },
  Sup { fst: Box<Term>, snd: Box<Term> },
  Era,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NumOper {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Xor,
  Shl,
  Shr,
  Ltn,
  Lte,
  Gtn,
  Gte,
  Eql,
  Neq,
}

impl From<NumOper> for hvm_core::OP {
  fn from(value: NumOper) -> Self {
    match value {
      NumOper::Add => hvm_core::OP::ADD,
      _ => todo!(),
    }
  }
}

impl From<hvm_core::OP> for NumOper {
  fn from(value: hvm_core::OP) -> Self {
    match value {
      hvm_core::OP::ADD => NumOper::Add,
      _ => todo!(),
    }
  }
}

impl From<&hvm_core::OP> for NumOper {
  fn from(value: &hvm_core::OP) -> Self {
    match value {
      hvm_core::OP::ADD => NumOper::Add,
      _ => todo!(),
    }
  }
}

impl DefinitionBook {
  pub fn new() -> Self {
    Default::default()
  }
}

impl From<Pattern> for Term {
  fn from(value: Pattern) -> Self {
    match value {
      Pattern::Ctr(nam, args) => {
        args.into_iter().fold(Term::Ref { def_id: DefId::from(&nam) }, |acc, arg| Term::App {
          fun: Box::new(acc),
          arg: Box::new(arg.into()),
        })
      }
      Pattern::Num(num) => Term::Num { val: num },
      Pattern::Var(nam) => {
        if let Some(nam) = nam {
          Term::Var { nam }
        } else {
          Term::Era
        }
      }
    }
  }
}

impl fmt::Display for NumOper {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NumOper::Add => write!(f, "+"),
      NumOper::Sub => write!(f, "-"),
      NumOper::Mul => write!(f, "*"),
      NumOper::Div => write!(f, "/"),
      NumOper::Mod => write!(f, "%"),
      NumOper::And => write!(f, "&"),
      NumOper::Or => write!(f, "|"),
      NumOper::Xor => write!(f, "^"),
      NumOper::Shl => write!(f, "<<"),
      NumOper::Shr => write!(f, ">>"),
      NumOper::Ltn => write!(f, "<"),
      NumOper::Lte => write!(f, "<="),
      NumOper::Gtn => write!(f, ">"),
      NumOper::Gte => write!(f, ">="),
      NumOper::Eql => write!(f, "=="),
      NumOper::Neq => write!(f, "!="),
    }
  }
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Term::Lam { nam, bod } => write!(f, "λ{} {}", nam.clone().unwrap_or(Name("*".to_string())), bod),
      Term::Var { nam } => write!(f, "{nam}"),
      Term::GlobalLam { nam, bod } => write!(f, "λ${nam} {bod}"),
      Term::GlobalVar { nam } => write!(f, "${nam}"),
      Term::Ref { def_id } => write!(f, "{}", Name::from(*def_id)),
      Term::App { fun, arg } => write!(f, "({fun} {arg})"),
      Term::Dup { fst, snd, val, nxt } => write!(
        f,
        "dup {} {} = {}; {}",
        fst.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        snd.as_ref().map(|x| x.as_str()).unwrap_or("*"),
        val,
        nxt
      ),
      Term::Num { val } => write!(f, "{val}"),
      Term::NumOp { op, fst, snd } => write!(f, "({op} {fst} {snd})"),
      Term::Sup { fst, snd } => write!(f, "{{{fst} {snd}}}"),
      Term::Era => write!(f, "*"),
    }
  }
}

impl fmt::Display for Pattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", Term::from(self.clone()))
  }
}

impl fmt::Display for Rule {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Rule { def_id, pats, body } = self;
    writeln!(f, "({}{}) = {}", Name::from(*def_id), pats.iter().map(|x| format!(" {x}")).join(""), body)
  }
}

impl fmt::Display for Definition {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for rule in &self.rules {
      write!(f, "{rule}")?
    }
    Ok(())
  }
}

impl fmt::Display for DefinitionBook {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.defs.values().map(|x| x.to_string()).join("\n"))
  }
}
