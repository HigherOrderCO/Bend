pub type Range = std::ops::Range<usize>;

const GHOST_RANGE: Range = 0 .. 0;

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

  pub fn mix(&self, other: Spanned<T>) -> Range {
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
