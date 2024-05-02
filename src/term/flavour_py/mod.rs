mod order_kwargs;
pub mod parser;
pub mod to_lang;

use indexmap::IndexMap;
use interner::global::GlobalString;

use crate::term::Name;

use super::Op;

#[derive(Clone, Debug)]
pub enum Term {
  // "None"
  None,
  // [a-zA-Z_]+
  Var { nam: Name },
  // [0-9_]+
  Num { val: u32 },
  // {fun}(args,)
  Call { fun: Box<Term>, args: Vec<Term>, kwargs: Vec<(Name, Term)> },
  // "lambda" {pat}* ":" {bod}
  Lam { names: Vec<Name>, bod: Stmt },
  // {lhs} {op} {rhs}
  Bin { op: Op, lhs: Box<Term>, rhs: Box<Term> },
  // "\"" ... "\""
  Str { val: GlobalString },
  // "[" ... "]"
  Lst { els: Vec<Term> },
  // "(" ... ")"
  Tup { els: Vec<Term> },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
  pub lft: Option<Name>,
  pub rgt: Stmt,
}

#[derive(Clone, Debug)]
pub enum AssignPattern {
  // [a-zA-Z_]+
  Var(Name),
  // "(" ... ")"
  Tup(Vec<Name>),
}

#[derive(Clone, Debug)]
pub enum Stmt {
  // {pat} = {val} ";" {nxt}
  Assign { pat: AssignPattern, val: Box<Term>, nxt: Box<Stmt> },
  // "if" {cond} ":"
  //  {then}
  // "else" ":"
  //  {otherwise}
  If { cond: Box<Term>, then: Box<Stmt>, otherwise: Box<Stmt> },
  // "match" {arg} ":" ("as" {bind})?
  //   case {lft} ":" {rgt}
  Match { arg: Box<Term>, bind: Option<Name>, arms: Vec<MatchArm> },
  // "switch" {arg} ("as" {bind})?
  //   case 0..wildcard ":" {rgt}
  Switch { arg: Box<Term>, bind: Option<Name>, arms: Vec<Stmt> },
  // "fold" {fun} {arg} ("as" {bind})? ":" {arms}
  //   case {lft} ":" {rgt}
  Fold { fun: Name, arg: Box<Term>, bind: Option<Name>, arms: Vec<MatchArm> },
  // "do" {fun} ":" {block}
  Do { fun: Name, block: Vec<MBind> },
  // "return" {expr} ";"
  Return { term: Box<Term> },
}

#[derive(Clone, Debug)]
pub enum MBind {
  Ask { pat: AssignPattern, val: Box<Term> },
  Stmt { stmt: Box<Stmt> },
}

// Name "(" {fields}* ")"
#[derive(Clone, Debug)]
pub struct Variant {
  pub name: Name,
  pub fields: Vec<Name>,
}

// "def" {name} "(" {params} ")" ":" {body}
#[derive(Clone, Debug)]
pub struct Definition {
  pub name: Name,
  pub params: Vec<Name>,
  pub body: Stmt,
}

// "enum" ":" {variants}*
#[derive(Clone, Debug)]
pub struct Enum {
  pub name: Name,
  pub variants: IndexMap<Name, Variant>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
  Def(Definition),
  Enum(Enum),
}

#[derive(Debug, Clone)]
pub struct Program {
  pub enums: IndexMap<Name, Enum>,
  pub defs: IndexMap<Name, Definition>,
  pub variants: IndexMap<Name, Name>,
}
