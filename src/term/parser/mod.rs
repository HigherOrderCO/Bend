pub mod lexer;
#[allow(clippy::module_inception)]
pub mod parser;

pub use parser::{parse_definition_book, parse_term};
