use std::fmt::Debug;

pub type Range = std::ops::Range<usize>;

const GHOST_RANGE: Range = 0 .. 0;

#[derive(Clone)]
pub struct Spanned<T> {
  pub inner: T,
  pub span: Range,
}

impl<T> Spanned<T> {
  pub fn new(inner: T, span: Range) -> Self {
    Self { inner, span }
  }

  pub fn ghost(inner: T) -> Self {
    Self { inner, span: GHOST_RANGE }
  }

  pub fn mix<Other>(&self, other: &Spanned<Other>) -> Range {
    self.span.start .. other.span.end
  }
}

impl<T> std::ops::Deref for Spanned<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.inner
  }
}

impl<T> AsRef<T> for Spanned<T> {
  fn as_ref(&self) -> &T {
    &self.inner
  }
}

impl<T: std::fmt::Debug> Debug for Spanned<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.inner.fmt(f)
  }
}

impl<T> From<T> for Spanned<T> {
  fn from(value: T) -> Self {
    Spanned::ghost(value)
  }
}

impl<T> std::ops::DerefMut for Spanned<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.inner
  }
}
