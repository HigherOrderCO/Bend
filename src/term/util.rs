// TODO: Find a way to implement Clone for ChildrenIter

/// Iterator for tree children
pub enum ChildrenIter<'a, T: 'a> {
  Zero(std::array::IntoIter<T, 0>),
  One(std::array::IntoIter<T, 1>),
  Two(std::array::IntoIter<T, 2>),
  Many(Box<dyn DoubleEndedIterator<Item = T> + 'a>),
}

impl<'a, T: 'a> ChildrenIter<'a, T> {
  pub fn zero() -> Self {
    ChildrenIter::Zero([].into_iter())
  }

  pub fn one(value: T) -> Self {
    ChildrenIter::One([value].into_iter())
  }

  pub fn two(fst: T, snd: T) -> Self {
    ChildrenIter::Two([fst, snd].into_iter())
  }
}

impl<'a, T: 'a> Iterator for ChildrenIter<'a, T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    match self {
      ChildrenIter::Zero(it) => it.next(),
      ChildrenIter::One(it) => it.next(),
      ChildrenIter::Two(it) => it.next(),
      ChildrenIter::Many(it) => it.next(),
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    match self {
      ChildrenIter::Zero(it) => it.size_hint(),
      ChildrenIter::One(it) => it.size_hint(),
      ChildrenIter::Two(it) => it.size_hint(),
      ChildrenIter::Many(it) => it.size_hint(),
    }
  }
}

impl<'a, T: 'a> DoubleEndedIterator for ChildrenIter<'a, T> {
  fn next_back(&mut self) -> Option<Self::Item> {
    match self {
      ChildrenIter::Zero(it) => it.next_back(),
      ChildrenIter::One(it) => it.next_back(),
      ChildrenIter::Two(it) => it.next_back(),
      ChildrenIter::Many(it) => it.next_back(),
    }
  }
}
