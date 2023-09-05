use hvm2::lang::OP;

use super::{Name, Number};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct DefinitionBook {
  pub defs: HashMap<Name, Definition>,
}

#[derive(Debug, Clone)]
pub struct Definition {
  pub name: Name,
  pub rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
pub struct Rule {
  pub name: Name,
  pub pats: Vec<Pattern>,
  pub body: Term,
}

#[derive(Debug, Clone)]
pub enum Pattern {
  _Ctr(Name, Vec<Pattern>),
  _Num(Number),
  _Var(Name),
}

#[derive(Debug, Clone)]
pub enum Term {
  Lam { nam: Name, bod: Box<Term> },
  Var { nam: Name },
  App { fun: Box<Term>, arg: Box<Term> },
  Dup { fst: Name, snd: Name, val: Box<Term>, nxt: Box<Term> },
  Num { val: Number },
  NumOp { op: NumOper, fst: Box<Term>, snd: Box<Term> },
}

// TODO: Switch to the hvm2 type when it's done
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

impl From<NumOper> for u8 {
  fn from(value: NumOper) -> Self {
    match value {
      NumOper::Add => 0x0,
      NumOper::Sub => 0x1,
      NumOper::Mul => 0x2,
      NumOper::Div => 0x3,
      NumOper::Mod => 0x4,
      NumOper::And => 0x5,
      NumOper::Or => 0x6,
      NumOper::Xor => 0x7,
      NumOper::Shl => 0x8,
      NumOper::Shr => 0x9,
      NumOper::Ltn => 0xa,
      NumOper::Lte => 0xb,
      NumOper::Gtn => 0xc,
      NumOper::Gte => 0xd,
      NumOper::Eql => 0xe,
      NumOper::Neq => 0xf,
    }
  }
}

impl TryFrom<u8> for NumOper {
  type Error = ();

  fn try_from(value: u8) -> Result<Self, Self::Error> {
    match value {
      0 => Ok(NumOper::Add),
      1 => Ok(NumOper::Sub),
      2 => Ok(NumOper::Mul),
      3 => Ok(NumOper::Div),
      4 ..= 15 => todo!(),
      _ => Err(()),
    }
  }
}

impl From<NumOper> for OP {
  fn from(value: NumOper) -> Self {
    match value {
      NumOper::Add => OP::ADD,
      NumOper::Sub => OP::SUB,
      NumOper::Mul => OP::MUL,
      NumOper::Div => OP::DIV,
      NumOper::Mod => todo!(),
      NumOper::And => todo!(),
      NumOper::Or => todo!(),
      NumOper::Xor => todo!(),
      NumOper::Shl => todo!(),
      NumOper::Shr => todo!(),
      NumOper::Ltn => todo!(),
      NumOper::Lte => todo!(),
      NumOper::Gtn => todo!(),
      NumOper::Gte => todo!(),
      NumOper::Eql => todo!(),
      NumOper::Neq => todo!(),
    }
  }
}

impl DefinitionBook {
  pub fn new() -> Self {
    Default::default()
  }
}
