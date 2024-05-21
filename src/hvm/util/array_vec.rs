use arrayvec::ArrayVec;

struct Assert<const COND: bool>;

trait IsTrue {}

impl IsTrue for Assert<true> {}

#[allow(private_bounds)]
pub(crate) fn from_array<T, const LEN: usize, const CAP: usize>(array: [T; LEN]) -> ArrayVec<T, CAP>
where
  Assert<{ LEN <= CAP }>: IsTrue,
{
  let mut vec = ArrayVec::new();
  unsafe {
    for el in array {
      vec.push_unchecked(el)
    }
  }
  vec
}

pub(crate) fn from_iter<T, const CAP: usize>(iter: impl IntoIterator<Item = T>) -> ArrayVec<T, CAP> {
  let mut vec = ArrayVec::new();
  for item in iter {
    vec.push(item);
  }
  vec
}
