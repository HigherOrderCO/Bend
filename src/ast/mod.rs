pub mod compat;
pub mod core;
pub mod hvm_lang;
pub mod spanned;

pub use hvm_lang::{Definition, DefinitionBook, Rule, Term};

use derive_more::{Display, From, Into};
use hvmc::Val;
use shrinkwraprs::Shrinkwrap;

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash, PartialOrd, Ord, From, Into, Display)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, From, Into, Default)]
pub struct DefId(pub Val);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, From, Into)]
pub struct VarId(pub Val);

pub fn var_id_to_name(mut var_id: Val) -> Name {
  let mut name = String::new();
  loop {
    let c = (var_id % 26) as u8 + b'a';
    name.push(c as char);
    var_id /= 26;
    if var_id == 0 {
      break;
    }
  }
  Name(name)
}

impl Name {
  pub fn new(value: &str) -> Self {
    Name(value.to_string())
  }
}

// TODO: We use this workaround because hvm-core's val_to_name function doesn't work with value 0
impl DefId {
  pub fn to_internal(self) -> Val {
    *self + 1
  }

  pub fn from_internal(val: Val) -> Self {
    Self(val - 1)
  }
}
