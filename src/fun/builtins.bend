data String = (Cons head ~tail) | (Nil)
data List   = (Cons head ~tail) | (Nil)
data Result = (Ok val) | (Err val)
data Nat    = (Succ ~pred) | (Zero)

// MAP Impl

data Map = (Node value ~left ~right) | (Leaf)

Map/empty = Map/Leaf

Map/get map key =
  match map {
    Map/Leaf: (*, map)
    Map/Node:
      switch _ = (== 0 key) {
        0: switch _ = (% key 2) {
          0:
            let (got, rest) = (Map/get map.left (/ key 2))
            (got, (Map/Node map.value rest map.right))
          _:
            let (got, rest) = (Map/get map.right (/ key 2))
            (got, (Map/Node map.value map.left rest))
        }
        _: (map.value, map)
      }
  }

Map/set map key value =
  match map {
    Map/Node:
      switch _ = (== 0 key) {
        0: switch _ = (% key 2) {
          0: (Map/Node map.value (Map/set map.left (/ key 2) value) map.right)
          _: (Map/Node map.value map.left (Map/set map.right (/ key 2) value))
        }
        _: (Map/Node value map.left map.right)
      }
    Map/Leaf:
      switch _ = (== 0 key) {
        0: switch _ = (% key 2) {
          0: (Map/Node * (Map/set Map/Leaf (/ key 2) value) Map/Leaf)
          _: (Map/Node * Map/Leaf (Map/set Map/Leaf (/ key 2) value))
        }
        _: (Map/Node value Map/Leaf Map/Leaf)
      }
  }

// IO Impl

STRING_NIL_TAG  = 0
STRING_CONS_TAG = 1

IO_DONE_TAG       = 0
IO_PUT_TEXT_TAG   = 1
IO_GET_TEXT_TAG   = 2
IO_WRITE_FILE_TAG = 3
IO_READ_FILE_TAG  = 4
IO_GET_TIME_TAG   = 5
IO_SLEEP_TAG      = 6
IO_DRAW_IMAGE_TAG = 7

data IO
  = (Done term)
  | (PutText   text      cont)
  | (GetText             cont)
  | (WriteFile file data cont)
  | (ReadFile  file      cont)
  | (GetTime             cont)
  | (Sleep     time      cont)
  | (DrawImage tree      cont)

MkStr (String/Cons x xs) = λt (t STRING_CONS_TAG x (MkStr xs))
MkStr (String/Nil)       = λt (t STRING_NIL_TAG)

ReadStr s = (s λtag switch tag {
  0: String/Nil
  _: λx λxs (String/Cons x (ReadStr xs))
})

MkIO (IO/Done term)                = λt (t IO_DONE_TAG term)
MkIO (IO/PutText text cont)        = λt (t IO_PUT_TEXT_TAG text λx (MkIO (cont x)))
MkIO (IO/GetText cont)             = λt (t IO_GET_TEXT_TAG λx (MkIO (cont x)))
MkIO (IO/WriteFile file data cont) = λt (t IO_WRITE_FILE_TAG file data λx (MkIO (cont x)))
MkIO (IO/ReadFile file cont)       = λt (t IO_READ_FILE_TAG file λx(MkIO (cont x)))
MkIO (IO/GetTime cont)             = λt (t IO_GET_TIME_TAG λx (MkIO (cont x)))
MkIO (IO/Sleep time cont)          = λt (t IO_SLEEP_TAG time λx (MkIO (cont x)))
MkIO (IO/DrawImage tree cont)      = λt (t IO_DRAW_IMAGE_TAG tree λx (MkIO (cont x)))

ReadIO io = (io λtag switch tag {
  0: λterm (IO/Done term)
  1: λtext (IO/PutText (ReadStr text) λcont (ReadIO cont))
  2: (IO/GetText λcont (ReadIO cont))
  3: λfile λdata (IO/WriteFile file data λcont (ReadIO cont))
  4: λfile (IO/ReadFile file λcont (ReadIO cont))
  5: (IO/GetTime λcont (ReadIO cont))
  6: λtime (IO/Sleep time λcont (ReadIO cont))
  7: λtree (IO/DrawImage tree λcont (ReadIO cont))
  _: *
})
