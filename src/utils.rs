/// A macro for creating iterators that can have statically known
/// different types. Useful for iterating over tree children, where
/// each tree node variant yields a different iterator type.
#[macro_export]
macro_rules! multi_iterator {
  ($Iter:ident { $($Variant:ident),* $(,)? }) => {
    #[derive(Debug, Clone)]
    enum $Iter<$($Variant),*> {
      $($Variant { iter: $Variant }),*
    }

    impl<$($Variant),*> $Iter<$($Variant),*> {
      $(
        #[allow(non_snake_case)]
        fn $Variant(iter: impl IntoIterator<IntoIter = $Variant>) -> Self {
          $Iter::$Variant { iter: iter.into_iter() }
        }
      )*
    }

    impl<T, $($Variant: Iterator<Item = T>),*> Iterator for $Iter<$($Variant),*> {
      type Item = T;
      fn next(&mut self) -> Option<T> {
        match self { $($Iter::$Variant { iter } => iter.next()),* }
      }

      fn size_hint(&self) -> (usize, Option<usize>) {
        match self { $($Iter::$Variant { iter } => iter.size_hint()),* }
      }
    }

    impl<T, $($Variant: DoubleEndedIterator<Item = T>),*> DoubleEndedIterator for $Iter<$($Variant),*> {
      fn next_back(&mut self) -> Option<T> {
        match self { $($Iter::$Variant { iter } => iter.next_back()),* }
      }
    }
  };
}
