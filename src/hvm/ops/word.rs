pub trait FromWord {
  fn from_word(bits: u64) -> Self;
}

pub trait ToWord {
  fn to_word(self) -> u64;
}

macro_rules! impl_word {
  ( $($ty:ty),+ ) => {
    $(
      impl FromWord for $ty {
        #[inline(always)]
        fn from_word(bits: u64) -> Self {
          bits as Self
        }
      }

      impl ToWord for $ty {
        #[inline(always)]
        fn to_word(self) -> u64 {
          self as u64
        }
      }
    )*
  };
}

impl_word! { u8, u16, u32, u64, i8, i16, i32 }

impl FromWord for f32 {
  #[inline(always)]
  fn from_word(bits: u64) -> Self {
    f32::from_bits(bits as u32)
  }
}

impl ToWord for f32 {
  #[inline(always)]
  fn to_word(self) -> u64 {
    self.to_bits() as u64
  }
}
