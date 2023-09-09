pub mod compat;
pub mod core;
pub mod hvm_lang;

use hvm_core::{name_to_u64, u64_to_name};
pub use hvm_lang::{Definition, DefinitionBook, NumOper, Rule, Term};

use derive_more::{Display, From, Into};
use shrinkwraprs::Shrinkwrap;

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash, PartialOrd, Ord, From, Into, Display)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, From, Into)]
pub struct DefId(pub u64);

// TODO: Accept other number types
#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Display)]
pub struct Number(pub u64);

impl From<&Name> for DefId {
  fn from(value: &Name) -> Self {
    DefId::from(value.as_str())
  }
}

impl From<&str> for DefId {
  fn from(value: &str) -> Self {
    name_to_u64(value).into()
  }
}

impl From<DefId> for Name {
  fn from(value: DefId) -> Self {
    Name(u64_to_name(*value))
  }
}
