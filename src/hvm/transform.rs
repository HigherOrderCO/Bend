use core::fmt;

pub mod eta_reduce;
pub mod inline;
pub mod prune;

pub enum TransformError {
  InfiniteRefCycle(String),
}

impl fmt::Display for TransformError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TransformError::InfiniteRefCycle(r) => write!(f, "infinite reference cycle in `@{r}`"),
    }
  }
}
