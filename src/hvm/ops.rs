mod num;
mod word;

use self::{
  num::Numeric,
  word::{FromWord, ToWord},
};
use core::{
  cmp::{Eq, Ord},
  fmt,
};
use std::mem;

use super::util::bi_enum;

bi_enum! {
  #[repr(u8)]
  /// The type of a numeric operation.
  ///
  /// This dictates how the bits of the operands will be interpreted,
  /// and the return type of the operation.
  #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub enum Ty {
    "u8":  U8  = 0,
    "u16": U16 = 1,
    "u32": U32 = 2,
    "u60": U60 = 3,
    "i8":  I8  = 4,
    "i16": I16 = 5,
    "i32": I32 = 6,
    "f32": F32 = 7,
  }
}

impl Ty {
  #[inline(always)]
  fn is_int(&self) -> bool {
    *self < Self::F32
  }
}

bi_enum! {
  #[repr(u8)]
  /// Native operations on numerics (u8, u16, u32, u60, i8, i16, i32, f32).
  ///
  /// Each operation has a swapped counterpart (accessible with `.swap()`),
  /// where the order of the operands is swapped.
  ///
  /// Operations without an already-named counterpart (e.g. `Add <-> Add` and
  /// `Lt <-> Gt`) are suffixed with `$`/`S`: `(-$ 1 2) = (- 2 1) = 1`.
  #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub enum Op {
    "+":   Add  = 0,
    "-":   Sub  = 1,
    "-$":  SubS = 2,
    "*":   Mul  = 3,
    "/":   Div  = 4,
    "/$":  DivS = 5,
    "%":   Rem  = 6,
    "%$":  RemS = 7,
    "&":   And  = 8,
    "|":   Or   = 9,
    "^":   Xor  = 10,
    "<<":  Shl  = 11,
    "<<$": ShlS = 12,
    ">>":  Shr  = 13,
    ">>$": ShrS = 14,
    // operators returning ints should go after `Eq`
    "==":  Eq   = 15,
    "!=":  Ne   = 16,
    "<":   Lt   = 17,
    ">":   Gt   = 18,
    "<=":  Le   = 19,
    ">=":  Ge   = 20,
  }
}

impl Op {
  /// Returns this operation's swapped counterpart.
  ///
  /// For all `op, a, b`, `op.swap().op(a, b) == op.op(b, a)`.
  #[inline]
  pub fn swap(self) -> Self {
    match self {
      Self::Add => Self::Add,
      Self::Sub => Self::SubS,
      Self::SubS => Self::Sub,
      Self::Mul => Self::Mul,
      Self::Div => Self::DivS,
      Self::DivS => Self::Div,
      Self::Rem => Self::RemS,
      Self::RemS => Self::Rem,
      Self::And => Self::And,
      Self::Or => Self::Or,
      Self::Xor => Self::Xor,
      Self::Shl => Self::ShlS,
      Self::ShlS => Self::Shl,
      Self::Shr => Self::ShrS,
      Self::ShrS => Self::Shr,
      Self::Eq => Self::Eq,
      Self::Ne => Self::Ne,
      Self::Lt => Self::Gt,
      Self::Gt => Self::Lt,
      Self::Le => Self::Ge,
      Self::Ge => Self::Le,
    }
  }

  fn op<T: Numeric + FromWord + ToWord>(self, a: u64, b: u64) -> u64 {
    let a = T::from_word(a);
    let b = T::from_word(b);

    match self {
      Self::Add => T::add(a, b).to_word(),
      Self::Sub => T::sub(a, b).to_word(),
      Self::SubS => T::sub(b, a).to_word(),
      Self::Mul => T::mul(a, b).to_word(),
      Self::Div => T::div(a, b).to_word(),
      Self::DivS => T::div(b, a).to_word(),
      Self::Rem => T::rem(a, b).to_word(),
      Self::RemS => T::rem(b, a).to_word(),
      Self::And => T::and(a, b).to_word(),
      Self::Or => T::or(a, b).to_word(),
      Self::Xor => T::xor(a, b).to_word(),
      Self::Shl => T::shl(a, b).to_word(),
      Self::ShlS => T::shl(b, a).to_word(),
      Self::Shr => T::shr(a, b).to_word(),
      Self::ShrS => T::shr(b, a).to_word(),

      // comparison operators return an integer, which is not necessarily a `T`.
      Self::Eq => (a == b).into(),
      Self::Ne => (a != b).into(),
      Self::Lt => (a < b).into(),
      Self::Le => (a <= b).into(),
      Self::Gt => (a > b).into(),
      Self::Ge => (a >= b).into(),
    }
  }

  #[inline(always)]
  fn is_comparison(&self) -> bool {
    *self >= Self::Eq
  }
}

/// A numeric operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C, align(2))]
pub struct TypedOp {
  /// The type of the operands.
  pub ty: Ty,
  /// The operation. An opaque type whose interpretation depends on `ty`.
  pub op: Op,
}

impl TypedOp {
  pub unsafe fn from_unchecked(val: u16) -> Self {
    mem::transmute(val)
  }

  /// Whether this operation returns an int.
  #[inline(always)]
  pub fn is_int(&self) -> bool {
    self.ty.is_int() || self.op.is_comparison()
  }

  pub fn swap(self) -> Self {
    Self { op: self.op.swap(), ty: self.ty }
  }

  #[inline]
  pub fn op(self, a: u64, b: u64) -> u64 {
    const U60: u64 = 0xFFF_FFFF_FFFF_FFFF;

    match self.ty {
      Ty::I8 => self.op.op::<i8>(a, b),
      Ty::I16 => self.op.op::<i16>(a, b),
      Ty::I32 => self.op.op::<i32>(a, b),

      Ty::U8 => self.op.op::<u8>(a, b),
      Ty::U16 => self.op.op::<u16>(a, b),
      Ty::U32 => self.op.op::<u32>(a, b),
      Ty::U60 => self.op.op::<u64>(a, b) & U60,

      Ty::F32 => self.op.op::<f32>(a, b),
    }
  }
}

impl fmt::Display for TypedOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::U60 => write!(f, "{}", self.op),
      _ => write!(f, "{}.{}", self.ty, self.op),
    }
  }
}

impl TryFrom<u16> for TypedOp {
  type Error = ();

  fn try_from(value: u16) -> Result<Self, Self::Error> {
    let [ty, op] = value.to_ne_bytes();

    Ok(Self { ty: Ty::try_from(ty)?, op: Op::try_from(op)? })
  }
}

impl From<TypedOp> for u16 {
  fn from(TypedOp { ty, op }: TypedOp) -> Self {
    u16::from_ne_bytes([ty as u8, op as u8])
  }
}

// #[cfg_attr(feature = "std", derive(Error))]
// #[derive(Debug)]
// pub enum OpParseError {
//   #[cfg_attr(feature = "std", error("invalid type: {0}"))]
//   Type(String),
//   #[cfg_attr(feature = "std", error("invalid operator: {0}"))]
//   Op(String),
// }
