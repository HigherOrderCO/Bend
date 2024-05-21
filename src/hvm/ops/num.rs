#[rustfmt::skip]
pub trait Numeric: PartialEq + PartialOrd + Sized {
  const ZERO: Self;

  fn add(_: Self, _: Self) -> Self { Self::ZERO }
  fn sub(_: Self, _: Self) -> Self { Self::ZERO }
  fn mul(_: Self, _: Self) -> Self { Self::ZERO }
  fn div(_: Self, _: Self) -> Self { Self::ZERO }
  fn rem(_: Self, _: Self) -> Self { Self::ZERO }
  fn and(_: Self, _: Self) -> Self { Self::ZERO }
  fn or(_: Self, _: Self) -> Self { Self::ZERO }
  fn xor(_: Self, _: Self) -> Self { Self::ZERO }
  fn shl(_: Self, _: Self) -> Self { Self::ZERO }
  fn shr(_: Self, _: Self) -> Self { Self::ZERO }
}

macro_rules! impl_numeric {
  ( $($ty:ty),+ ) => {
    $(
      impl Numeric for $ty {
        const ZERO: Self = 0;

        fn add(a: Self, b: Self) -> Self { a.wrapping_add(b) }
        fn sub(a: Self, b: Self) -> Self { a.wrapping_sub(b) }
        fn mul(a: Self, b: Self) -> Self { a.wrapping_mul(b) }
        fn div(a: Self, b: Self) -> Self { a.checked_div(b).unwrap_or(0) }
        fn rem(a: Self, b: Self) -> Self { a.checked_rem(b).unwrap_or(0) }
        fn and(a: Self, b: Self) -> Self { a & b }
        fn or(a: Self, b: Self) -> Self { a | b }
        fn xor(a: Self, b: Self) -> Self { a ^ b }
        fn shl(a: Self, b: Self) -> Self { a.wrapping_shl(b as u32) }
        fn shr(a: Self, b: Self) -> Self { a.wrapping_shr(b as u32) }
      }
    )*
  }
}

impl_numeric! { u8, u16, u32, u64, i8, i16, i32 }

impl Numeric for f32 {
  const ZERO: Self = 0.0;

  fn add(a: Self, b: Self) -> Self {
    a + b
  }
  fn sub(a: Self, b: Self) -> Self {
    a - b
  }
  fn mul(a: Self, b: Self) -> Self {
    a * b
  }
  fn div(a: Self, b: Self) -> Self {
    a / b
  }
  fn rem(a: Self, b: Self) -> Self {
    a % b
  }
}
