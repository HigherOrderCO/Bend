//! The textual language of HVMC.
//!
//! This file defines an AST for interaction nets, and functions to convert this
//! AST to/from the textual syntax.
//!
//! The grammar is documented in the repo README, as well as within the parser
//! methods, for convenience.
//!
//! The AST is based on the [interaction calculus].
//!
//! [interaction calculus]: https://en.wikipedia.org/wiki/Interaction_nets#Interaction_calculus

use arrayvec::ArrayVec;
use core::fmt;
use std::{collections::BTreeMap, iter, mem, str::FromStr};
use TSPL::Parser;

use crate::maybe_grow;

use super::util::{array_vec, deref};

/// The top level AST node, representing a collection of named nets.
///
/// This is a newtype wrapper around a `BTreeMap<String, Net>`, and is
/// dereferencable to such.
#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct Book {
  pub nets: BTreeMap<String, Net>,
}

deref!(Book => self.nets: BTreeMap<String, Net>);

/// An AST node representing an interaction net with one free port.
///
/// The tree connected to the free port is stored in `root`. The active pairs in
/// the net -- trees connected by their roots -- are stored in `redexes`.
///
/// (The wiring connecting the leaves of all the trees is represented within the
/// trees via pairs of [`Tree::Var`] nodes with the same name.)
#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct Net {
  pub root: Tree,
  pub redexes: Vec<(bool, Tree, Tree)>,
}

/// An AST node representing an interaction net tree.
///
/// Trees in interaction nets are inductively defined as either wires, or an
/// agent with all of its auxiliary ports (if any) connected to trees.
///
/// Here, the wires at the leaves of the tree are represented with
/// [`Tree::Var`], where the variable name is shared between both sides of the
/// wire.
#[derive(Hash, PartialEq, Eq, Debug, Default)]
pub enum Tree {
  #[default]
  /// A nilary eraser node.
  Era,
  /// A native 60-bit integer.
  Num { val: u32 },
  /// A nilary node, referencing a named net.
  Ref { nam: String },
  /// A n-ary interaction combinator.
  Ctr {
    /// The label of the combinator. (Combinators with the same label
    /// annihilate, and combinators with different labels commute.)
    lab: u16,
    /// The auxiliary ports of this node.
    ///
    /// - 0 ports: this behaves identically to an eraser node.
    /// - 1 port: this behaves identically to a wire.
    /// - 2 ports: this is a standard binary combinator node.
    /// - 3+ ports: equivalent to right-chained binary nodes; `(a b c)` is
    ///   equivalent to `(a (b c))`.
    ///
    /// The length of this vector must be less than [`MAX_ARITY`].
    ports: Vec<Tree>,
  },
  /// A binary node representing an operation on native integers.
  ///
  /// The principal port connects to the left operand.
  Op {
    /// An auxiliary port; connects to the right operand.
    fst: Box<Tree>,
    /// An auxiliary port; connects to the output.
    snd: Box<Tree>,
  },
  /// A binary node representing a match on native integers.
  ///
  /// The principal port connects to the integer to be matched on.
  Mat {
    /// An auxiliary port; connects to the zero branch.
    zero: Box<Tree>,
    /// An auxiliary port; connects to the a CTR with label 0 containing the
    /// predecessor and the output of the succ branch.
    succ: Box<Tree>,
    /// An auxiliary port; connects to the output.
    out: Box<Tree>,
  },
  /// One side of a wire; the other side will have the same name.
  Var { nam: String },
}

pub const MAX_ARITY: usize = 8;
pub const MAX_ADT_VARIANTS: usize = MAX_ARITY - 1;
pub const MAX_ADT_FIELDS: usize = MAX_ARITY - 1;

impl Net {
  pub fn trees(&self) -> impl Iterator<Item = &Tree> {
    iter::once(&self.root).chain(self.redexes.iter().flat_map(|(_, x, y)| [x, y]))
  }
  pub fn trees_mut(&mut self) -> impl Iterator<Item = &mut Tree> {
    iter::once(&mut self.root).chain(self.redexes.iter_mut().flat_map(|(_, x, y)| [x, y]))
  }
}

impl Tree {
  #[inline(always)]
  pub fn children(&self) -> impl ExactSizeIterator + DoubleEndedIterator<Item = &Tree> {
    ArrayVec::<_, MAX_ARITY>::into_iter(match self {
      Tree::Era | Tree::Num { .. } | Tree::Ref { .. } | Tree::Var { .. } => array_vec::from_array([]),
      Tree::Ctr { ports, .. } => array_vec::from_iter(ports),
      Tree::Op { fst: rhs, snd: out, .. } => array_vec::from_array([rhs, out]),
      Tree::Mat { zero, succ, out } => array_vec::from_array([zero, succ, out]),
    })
  }

  #[inline(always)]
  pub fn children_mut(&mut self) -> impl ExactSizeIterator + DoubleEndedIterator<Item = &mut Tree> {
    ArrayVec::<_, MAX_ARITY>::into_iter(match self {
      Tree::Era | Tree::Num { .. } | Tree::Ref { .. } | Tree::Var { .. } => array_vec::from_array([]),
      Tree::Ctr { ports, .. } => array_vec::from_iter(ports),
      Tree::Op { fst, snd } => array_vec::from_array([fst, snd]),
      Tree::Mat { zero, succ, out } => array_vec::from_array([zero, succ, out]),
    })
  }

  #[allow(unused)]
  pub(crate) fn lab(&self) -> Option<u16> {
    match self {
      Tree::Ctr { lab, ports } if ports.len() >= 2 => Some(*lab),
      _ => None,
    }
  }

  pub fn legacy_mat(mut arms: Tree, out: Tree) -> Option<Tree> {
    let Tree::Ctr { lab: 0, ports } = &mut arms else { None? };
    let ports = mem::take(ports);
    let Ok([zero, succ]) = <[_; 2]>::try_from(ports) else { None? };
    let zero = Box::new(zero);
    let succ = Box::new(succ);
    Some(Tree::Mat { zero, succ, out: Box::new(out) })
  }
}

pub const TY_SYM : u32 = 0x00;
pub const TY_U24 : u32 = 0x01;
pub const TY_I24 : u32 = 0x02;
pub const TY_F24 : u32 = 0x03;
pub const OP_ADD : u32 = 0x04;
pub const OP_SUB : u32 = 0x05;
pub const FP_SUB : u32 = 0x06;
pub const OP_MUL : u32 = 0x07;
pub const OP_DIV : u32 = 0x08;
pub const FP_DIV : u32 = 0x09;
pub const OP_REM : u32 = 0x0A;
pub const FP_REM : u32 = 0x0B;
pub const OP_EQ  : u32 = 0x0C;
pub const OP_NEQ : u32 = 0x0D;
pub const OP_LT  : u32 = 0x0E;
pub const OP_GT  : u32 = 0x0F;
pub const OP_AND : u32 = 0x10;
pub const OP_OR  : u32 = 0x11;
pub const OP_XOR : u32 = 0x12;
pub const OP_SHL : u32 = 0x13;
pub const FP_SHL : u32 = 0x14;
pub const OP_SHR : u32 = 0x15;
pub const FP_SHR : u32 = 0x16;

pub fn flip_sym(val: u32) -> u32 {
  match val {
    OP_ADD => OP_ADD,
    OP_SUB => FP_SUB,
    OP_MUL => OP_MUL,
    OP_DIV => FP_DIV,
    OP_REM => FP_REM,
    OP_EQ => OP_EQ,
    OP_NEQ => OP_NEQ,
    OP_LT => OP_GT,
    OP_GT => OP_LT,
    OP_AND => OP_AND,
    OP_OR => OP_OR,
    OP_XOR => OP_XOR,
    FP_SUB => OP_SUB,
    FP_DIV => OP_DIV,
    FP_REM => OP_REM,
    OP_SHL => FP_SHL,
    OP_SHR => FP_SHR,
    FP_SHL => OP_SHL,
    FP_SHR => OP_SHR,
    _ => unreachable!(),
  }
}

pub fn new_sym(val: u32) -> u32 {
  ((val as u8 as u32) << 5) | TY_SYM
}

pub fn get_sym(val: u32) -> u32 {
  (val >> 5) as u8 as u32
}

pub fn new_u24(val: u32) -> u32 {
  (val << 5) | TY_U24
}

pub fn get_u24(val: u32) -> u32 {
  val >> 5
}

pub fn new_i24(val: i32) -> u32 {
  ((val as u32) << 5) | TY_I24
}

pub fn get_i24(val: u32) -> i32 {
  ((val as i32) << 3) >> 8
}

pub fn new_f24(val: f32) -> u32 {
  let bits = val.to_bits();
  let mut shifted_bits = bits >> 8;
  let lost_bits = bits & 0xFF;
  shifted_bits += (lost_bits - ((lost_bits >> 7) & !shifted_bits)) >> 7; // round ties to even
  shifted_bits |= u32::from((bits & 0x7F800000 == 0x7F800000) && (bits << 9 != 0)); // ensure NaNs don't become infinities
  (shifted_bits << 5) | TY_F24
}

pub fn get_f24(val: u32) -> f32 {
  f32::from_bits((val << 3) & 0xFFFFFF00)
}

pub fn get_typ(val: u32) -> u32 {
  return (val & 0x1F) as u8 as u32;
}

pub fn partial_opr(a: u32, b: u32) -> u32 {
  (b & !0x1F) | get_sym(a)
}

impl fmt::Display for Book {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, (name, net)) in self.iter().enumerate() {
      if i != 0 {
        f.write_str("\n\n")?;
      }
      write!(f, "@{name} = {net}")?;
    }
    Ok(())
  }
}

impl fmt::Display for Net {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &self.root)?;
    for (pri, a, b) in &self.redexes {
      write!(f, "\n  &{} {a} ~ {b}", if *pri { "!" } else { "" })?;
    }
    Ok(())
  }
}

impl fmt::Display for Tree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    maybe_grow(move || match self {
      Tree::Era => write!(f, "*"),
      Tree::Ctr { lab, ports } => {
        match lab {
          0 => write!(f, "("),
          1 => write!(f, "{{"),
          _ => unreachable!(),
        }?;
        let mut space = *lab > 1;
        for port in ports {
          if space {
            write!(f, " ")?;
          }
          write!(f, "{port}")?;
          space = true;
        }
        match lab {
          0 => write!(f, ")"),
          1 => write!(f, "}}"),
          _ => unreachable!(),
        }?;
        Ok(())
      }
      Tree::Var { nam } => write!(f, "{nam}"),
      Tree::Ref { nam } => write!(f, "@{nam}"),
      Tree::Num { val } => {
        let numb = *val;
        match get_typ(numb) {
          TY_SYM => match get_sym(numb) as u8 as u32 {
            OP_ADD => write!(f, "[+]"),
            OP_SUB => write!(f, "[-]"),
            OP_MUL => write!(f, "[*]"),
            OP_DIV => write!(f, "[/]"),
            OP_REM => write!(f, "[%]"),
            OP_EQ  => write!(f, "[=]"),
            OP_LT  => write!(f, "[<]"),
            OP_GT  => write!(f, "[>]"),
            OP_AND => write!(f, "[&]"),
            OP_OR  => write!(f, "[|]"),
            OP_XOR => write!(f, "[^]"),
            OP_SHL => write!(f, "[<<]"),
            OP_SHR => write!(f, "[>>]"),
            FP_SUB => write!(f, "[:-]"),
            FP_DIV => write!(f, "[:/]"),
            FP_REM => write!(f, "[:%]"),
            FP_SHL => write!(f, "[:<<]"),
            FP_SHR => write!(f, "[:>>]"),
            _ => write!(f, "[?]"),
          }
          TY_U24 => {
            let val = get_u24(numb);
            write!(f, "{}", val)
          }
          TY_I24 => {
            let val = get_i24(numb);
            write!(f, "{:+}", val)
          }
          TY_F24 => {
            let val = get_f24(numb);
            match val {
              f32::INFINITY => write!(f, "+inf"),
              f32::NEG_INFINITY => write!(f, "-inf"),
              x if x.is_nan() => write!(f, "+NaN"),
              _ => write!(f, "{:?}", val)
            }
          }
          _ => {
            let typ = get_typ(numb);
            let val = get_u24(numb);
            write!(f, "[{}{}]", match typ {
              OP_ADD => "+",
              OP_SUB => "-", 
              OP_MUL => "*",
              OP_DIV => "/",
              OP_REM => "%",
              OP_EQ  => "=",
              OP_NEQ => "!",
              OP_LT  => "<",
              OP_GT  => ">",
              OP_AND => "&",
              OP_OR  => "|",
              OP_XOR => "^",
              OP_SHL => "<<",
              OP_SHR => ">>",
              FP_SUB => ":-",
              FP_DIV => ":/",
              FP_REM => ":%",
              FP_SHL => ":<<",
              FP_SHR => ":>>",
              _ => "?",
            }, val)
          }
        }
      }
      Tree::Op { fst, snd } => write!(f, "$({fst} {snd})"),
      Tree::Mat { zero, succ, out } => write!(f, "?(({zero} {succ}) {out})"),
    })
  }
}

// Manually implemented to avoid stack overflows.
impl Clone for Tree {
  fn clone(&self) -> Tree {
    maybe_grow(|| match self {
      Tree::Era => Tree::Era,
      Tree::Num { val } => Tree::Num { val: *val },
      Tree::Ref { nam } => Tree::Ref { nam: nam.clone() },
      Tree::Ctr { lab, ports } => Tree::Ctr { lab: *lab, ports: ports.clone() },
      Tree::Op { fst, snd } => Tree::Op { fst: fst.clone(), snd: snd.clone() },
      Tree::Mat { zero, succ, out } => Tree::Mat { zero: zero.clone(), succ: succ.clone(), out: out.clone() },
      Tree::Var { nam } => Tree::Var { nam: nam.clone() },
    })
  }
}

// Drops non-recursively to avoid stack overflows.
impl Drop for Tree {
  fn drop(&mut self) {
    loop {
      let mut i = self.children_mut().filter(|x| x.children().len() != 0);
      let Some(x) = i.next() else { break };
      if { i }.next().is_none() {
        // There's only one child; move it up to be the new root.
        *self = mem::take(x);
        continue;
      }
      // Rotate the tree right:
      // ```text
      //     a            b
      //    / \          / \
      //   b   e   ->   c   a
      //  / \              / \
      // c   d            d   e
      // ```
      let d = mem::take(x.children_mut().next_back().unwrap());
      let b = mem::replace(x, d);
      let a = mem::replace(self, b);
      mem::forget(mem::replace(self.children_mut().next_back().unwrap(), a));
    }
  }
}

// new_parser!(HvmcParser);
pub struct HvmcParser<'i> {
  input: &'i str,
  index: usize,
}

impl<'i> Parser<'i> for HvmcParser<'i> {
  fn input(&mut self) -> &'i str {
    self.input
  }

  fn index(&mut self) -> &mut usize {
    &mut self.index
  }
}

impl<'i> HvmcParser<'i> {
  pub fn new(input: &'i str) -> Self {
    Self { input, index: 0 }
  }
}

impl<'i> HvmcParser<'i> {
  /// Book = ("@" Name "=" Net)*
  fn parse_book(&mut self) -> Result<Book, String> {
    maybe_grow(move || {
      let mut book = BTreeMap::new();
      while self.consume("@").is_ok() {
        let name = self.parse_name()?;
        self.consume("=")?;
        let net = self.parse_net()?;
        book.insert(name, net);
      }
      Ok(Book { nets: book })
    })
  }

  /// Net = Tree ("&" Tree "~" Tree)*
  fn parse_net(&mut self) -> Result<Net, String> {
    let mut redexes = Vec::new();
    let root = self.parse_tree()?;
    while self.consume("&").is_ok() {
      let pri = if self.peek_one() == Some('!') {
        self.advance_one();
        true
      } else {
        false
      };
      let tree1 = self.parse_tree()?;
      self.consume("~")?;
      let tree2 = self.parse_tree()?;
      redexes.push((pri, tree1, tree2));
    }
    Ok(Net { root, redexes })
  }

  fn parse_tree(&mut self) -> Result<Tree, String> {
    maybe_grow(move || {
      self.skip_trivia();
      match self.peek_one() {
        Some('*') => {
          self.advance_one();
          Ok(Tree::Era)
        }
        Some('(') => {
          self.advance_one();
          let fst = self.parse_tree()?;
          self.skip_trivia();
          let snd = self.parse_tree()?;
          self.consume(")")?;
          Ok(Tree::Ctr { lab: 0, ports: vec![fst, snd] })
        }
        Some('{') => {
          self.advance_one();
          let fst = self.parse_tree()?;
          self.skip_trivia();
          let snd = self.parse_tree()?;
          self.consume("}")?;
          Ok(Tree::Ctr { lab: 1, ports: vec![fst, snd] })
        }
        Some('@') => {
          self.advance_one();
          self.skip_trivia();
          let nam = self.parse_name()?;
          Ok(Tree::Ref { nam })
        }
        Some('$') => {
          self.advance_one();
          self.consume("(")?;
          let fst = Box::new(self.parse_tree()?);
          self.skip_trivia();
          let snd = Box::new(self.parse_tree()?);
          self.consume(")")?;
          Ok(Tree::Op { fst, snd })
        }
        Some('?') => {
          self.advance_one();
          self.consume("(")?;
          let zero = self.parse_tree()?;
          let succ = self.parse_tree()?;
          self.skip_trivia();
          if self.peek_one() == Some(')') {
            self.advance_one();
            Tree::legacy_mat(zero, succ).ok_or_else(|| "invalid legacy match".to_owned())
          } else {
            let zero = Box::new(zero);
            let succ = Box::new(succ);
            let out = Box::new(self.parse_tree()?);
            self.consume(">")?;
            Ok(Tree::Mat { zero, succ, out })
          }
        }
        _ => {
          if let Some(c) = self.peek_one() {
            if "0123456789+-[".contains(c) {
              return Ok(Tree::Num { val: self.parse_numb()? });
            }
          }
          let nam = self.parse_name()?;
          Ok(Tree::Var { nam })
        }
      }
    })
  }

  pub fn parse_numb_sym(&mut self) -> Result<u32, String> {
    self.consume("[")?;

    // Parses the symbol
    let op = new_sym(match () {
      _ if self.try_consume("+") => OP_ADD,
      _ if self.try_consume("-") => OP_SUB,
      _ if self.try_consume("*") => OP_MUL,
      _ if self.try_consume("/") => OP_DIV,
      _ if self.try_consume("%") => OP_REM,
      _ if self.try_consume("=") => OP_EQ,
      _ if self.try_consume("!") => OP_NEQ,
      _ if self.try_consume("<") => OP_LT,
      _ if self.try_consume(">") => OP_GT,
      _ if self.try_consume("&") => OP_AND,
      _ if self.try_consume("|") => OP_OR,
      _ if self.try_consume("^") => OP_XOR,
      _ if self.try_consume(">>") => OP_SHR,
      _ if self.try_consume("<<") => OP_SHL,
      _ if self.try_consume(":-") => FP_SUB,
      _ if self.try_consume(":/") => FP_DIV,
      _ if self.try_consume(":%") => FP_REM,
      _ if self.try_consume(":>>") => FP_SHR,
      _ if self.try_consume(":<<") => FP_SHL,
      _ => self.expected("operator symbol")?,
    });
    self.skip_trivia();

    // Syntax for partial operations, like `[*2]`
    let num = if self.peek_one() != Some(']') { partial_opr(op, self.parse_numb_lit()?) } else { op };

    // Closes symbol bracket
    self.consume("]")?;

    // Returns the symbol
    return Ok(num);
  }

  pub fn parse_numb_lit(&mut self) -> Result<u32, String> {
    let num = self.take_while(|x| x.is_alphanumeric() || x == '+' || x == '-' || x == '.');

    Ok(if num.contains('.') || num.contains("inf") || num.contains("NaN") {
      let val: f32 = num.parse().map_err(|err| format!("invalid number literal: {err}"))?;
      new_f24(val)
    } else if num.starts_with('+') || num.starts_with('-') {
      let val = Self::parse_int(&num[1 ..])? as i32;
      new_i24(if num.starts_with('-') { -val } else { val })
    } else {
      let val = Self::parse_int(num)? as u32;
      new_u24(val)
    })
  }

  fn parse_int(input: &str) -> Result<u64, String> {
    if let Some(rest) = input.strip_prefix("0x") {
      u64::from_str_radix(rest, 16).map_err(|err| format!("{err:?}"))
    } else if let Some(rest) = input.strip_prefix("0b") {
      u64::from_str_radix(rest, 2).map_err(|err| format!("{err:?}"))
    } else {
      input.parse::<u64>().map_err(|err| format!("{err:?}"))
    }
  }

  pub fn parse_numb(&mut self) -> Result<u32, String> {
    self.skip_trivia();

    // Parses symbols (SYM)
    if let Some('[') = self.peek_one() {
      return self.parse_numb_sym();
    // Parses numbers (U24,I24,F24)
    } else {
      return self.parse_numb_lit();
    }
  }

  /// Name = /[a-zA-Z0-9_.$]+/
  fn parse_name(&mut self) -> Result<String, String> {
    let name = self.take_while(|c| c.is_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '/');
    if name.is_empty() {
      return self.expected("name");
    }
    Ok(name.to_owned())
  }

  fn try_consume(&mut self, str: &str) -> bool {
    let matches = self.peek_many(str.len()) == Some(str);
    if matches {
      self.advance_many(str.len());
    }
    matches
  }
}

/// Parses the input with the callback, ensuring that the whole input is
/// consumed.
fn parse_eof<'i, T>(input: &'i str, parse_fn: impl Fn(&mut HvmcParser<'i>) -> Result<T, String>) -> Result<T, String> {
  let mut parser = HvmcParser::new(input);
  let out = parse_fn(&mut parser)?;
  if parser.index != parser.input.len() {
    return Err("Unable to parse the whole input. Is this not an hvmc file?".to_owned());
  }
  Ok(out)
}

impl FromStr for Book {
  type Err = String;
  fn from_str(str: &str) -> Result<Self, Self::Err> {
    parse_eof(str, HvmcParser::parse_book)
  }
}

impl FromStr for Net {
  type Err = String;
  fn from_str(str: &str) -> Result<Self, Self::Err> {
    parse_eof(str, HvmcParser::parse_net)
  }
}

impl FromStr for Tree {
  type Err = String;
  fn from_str(str: &str) -> Result<Self, Self::Err> {
    parse_eof(str, HvmcParser::parse_tree)
  }
}
