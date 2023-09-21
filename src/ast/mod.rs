pub mod compat;
pub mod core;
pub mod hvm_lang;

pub use hvm_lang::{Definition, DefinitionBook, NumOper, Rule, Term};

use derive_more::{Display, From, Into};
use hvm_core::{name_to_val, val_to_name, Val};
use shrinkwraprs::Shrinkwrap;

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash, PartialOrd, Ord, From, Into, Display)]
pub struct Name(pub String);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, From, Into)]
pub struct DefId(pub Val);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord, From, Into)]
pub struct VarId(pub Val);

// TODO: Accept other number types
#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Display)]
pub struct Number(pub Val);

impl From<&Name> for DefId {
  fn from(value: &Name) -> Self {
    DefId::from(value.as_str())
  }
}

impl From<&str> for DefId {
  fn from(value: &str) -> Self {
    name_to_val(value).into()
  }
}

impl From<DefId> for Name {
  fn from(value: DefId) -> Self {
    Name(val_to_name(*value))
  }
}

pub fn name_to_id(name: &Name) -> Val {
  name_to_val(name)
}

pub fn id_to_name(num: Val) -> Name {
  Name(val_to_name(num))
}

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
