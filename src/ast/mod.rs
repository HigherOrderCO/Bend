pub mod core;
pub mod hvm_lang;

pub use hvm_lang::{Definition, DefinitionBook, NumOper, Rule, Term};

use shrinkwraprs::Shrinkwrap;

#[derive(Debug, PartialEq, Eq, Clone, Shrinkwrap, Hash)]
pub struct Name(pub String);

// TODO: Accept other number types
#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap)]
pub struct Number(pub u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Shrinkwrap, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);
