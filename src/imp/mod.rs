mod order_kwargs;
pub mod parser;
pub mod to_fun;

use indexmap::IndexMap;
use interner::global::GlobalString;

use crate::fun::{CtrField, Name, Op};

#[derive(Clone, Debug)]
pub enum Expr {
  // "None"
  None,
  // [a-zA-Z_]+
  Var { nam: Name },
  // [0-9_]+
  Num { val: u32 },
  // {fun}({args},{kwargs},)
  Call { fun: Box<Expr>, args: Vec<Expr>, kwargs: Vec<(Name, Expr)> },
  // "lambda" {names}* ":" {bod}
  Lam { names: Vec<Name>, bod: Box<Expr> },
  // {lhs} {op} {rhs}
  Bin { op: Op, lhs: Box<Expr>, rhs: Box<Expr> },
  // "\"" ... "\""
  Str { val: GlobalString },
  // "[" ... "]"
  Lst { els: Vec<Expr> },
  // "(" ... ")"
  Tup { els: Vec<Expr> },
  // "[" {term} "for" {bind} "in" {iter} ("if" {cond})? "]"
  Comprehension { term: Box<Expr>, bind: Name, iter: Box<Expr>, cond: Option<Box<Expr>> },
  // "{" {entries} "}"
  MapInit { entries: Vec<(MapKey, Expr)> },
  // {name} "[" {key} "]"
  MapGet { nam: Name, key: MapKey },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
  pub lft: Option<Name>,
  pub rgt: Stmt,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapKey(u32);

#[derive(Clone, Debug)]
pub enum AssignPattern {
  // [a-zA-Z_]+
  Var(Name),
  // "(" ... ")"
  Tup(Vec<Name>),
  // {name} "[" {key} "]"
  MapSet(Name, MapKey),
}

#[derive(Clone, Debug)]
pub enum InPlaceOp {
  Add,
  Sub,
  Mul,
  Div,
}

#[derive(Clone, Debug)]
pub enum Stmt {
  // {pat} = {val} ";" {nxt}
  Assign { pat: AssignPattern, val: Box<Expr>, nxt: Box<Stmt> },
  // {var} += {val} ";" {nxt}
  InPlace { op: InPlaceOp, var: Name, val: Box<Expr>, nxt: Box<Stmt> },
  // "if" {cond} ":"
  //  {then}
  // "else" ":"
  //  {otherwise}
  If { cond: Box<Expr>, then: Box<Stmt>, otherwise: Box<Stmt> },
  // "match" {arg} ":" ("as" {bind})?
  //   case {lft} ":" {rgt}
  Match { arg: Box<Expr>, bind: Option<Name>, arms: Vec<MatchArm> },
  // "switch" {arg} ("as" {bind})?
  //   case 0..wildcard ":" {rgt}
  Switch { arg: Box<Expr>, bind: Option<Name>, arms: Vec<Stmt> },
  // "bend" ({bind} ("="" {init})?)* "while" {cond} ":"
  //  {step}
  // "then" ":"
  //  {base}
  Bend { bind: Vec<Option<Name>>, init: Vec<Expr>, cond: Box<Expr>, step: Box<Stmt>, base: Box<Stmt> },
  // "fold" {arg} ("as" {bind})? ":" {arms}
  //   case {lft} ":" {rgt}
  Fold { arg: Box<Expr>, bind: Option<Name>, arms: Vec<MatchArm> },
  // "do" {fun} ":" {block}
  Do { fun: Name, block: Vec<MBind> },
  // "return" {expr} ";"
  Return { term: Box<Expr> },
}

#[derive(Clone, Debug)]
pub enum MBind {
  Ask { pat: AssignPattern, val: Box<Expr> },
  Stmt { stmt: Box<Stmt> },
}

// Name "(" {fields}* ")"
#[derive(Clone, Debug)]
pub struct Variant {
  pub name: Name,
  pub fields: Vec<CtrField>,
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

impl InPlaceOp {
  pub fn to_lang_op(self) -> Op {
    match self {
      InPlaceOp::Add => Op::ADD,
      InPlaceOp::Sub => Op::SUB,
      InPlaceOp::Mul => Op::MUL,
      InPlaceOp::Div => Op::DIV,
    }
  }
}
