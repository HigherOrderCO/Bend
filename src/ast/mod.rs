use std::collections::HashMap;

use shrinkwraprs::Shrinkwrap;

#[derive(Debug, Clone, Default)]
pub struct DefinitionBook {
  pub definitions: HashMap<Name, Definition>,
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
  Ctr(Name, Vec<Pattern>),
  Num(Number),
  Var(Name),
}

#[derive(Debug, Clone)]
pub enum Term {
  Lam { name: Name, body: Box<Term> },
  Var { name: Name },
  App { func: Box<Term>, arg: Box<Term> },
  Dup { fst: Name, snd: Name, val: Box<Term>, next: Box<Term> },
  Def { name: Name },
  Num { val: Number },
  NumOp { op: NumOper, fst: Box<Term>, snd: Box<Term> },
}

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash)]
pub struct Name(pub String);

// TODO: Accept other number types
#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap)]
pub struct Number(pub u16);

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

impl DefinitionBook {
  pub fn new() -> Self {
    Default::default()
  }
}
