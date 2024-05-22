type String = (Nil) | (Cons head ~tail)
type List   = (Nil) | (Cons head ~tail)
type Result = (Ok val) | (Err val)
type Nat    = (Succ ~pred) | (Zero)

# MAP Impl

type Map = (Node value ~left ~right) | (Leaf)

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

# IO Impl

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

type IO
  = (Done term)
  | (PutText   text      cont)
  | (GetText             cont)
  | (WriteFile file data cont)
  | (ReadFile  file      cont)
  | (GetTime             cont)
  | (Sleep     time      cont)
  | (DrawImage tree      cont)

IO/bind (IO/Done ret)                 f = (f ret)
IO/bind (IO/PutText text cont)        f = (IO/PutText text λx (IO/bind (cont x) f))
IO/bind (IO/GetText cont)             f = (IO/GetText λx (IO/bind (cont x) f))
IO/bind (IO/WriteFile file data cont) f = (IO/WriteFile file data λx (IO/bind (cont x) f))
IO/bind (IO/ReadFile file cont)       f = (IO/ReadFile file λx (IO/bind (cont x) f))
IO/bind (IO/GetTime cont)             f = (IO/GetTime λx (IO/bind (cont x) f))
IO/bind (IO/Sleep time cont)          f = (IO/Sleep time λx (IO/bind (cont x) f))
IO/bind (IO/DrawImage tree cont)      f = (IO/DrawImage tree λx (IO/bind (cont x) f))

print s = (IO/PutText s λx (IO/Done x))
get_time = (IO/GetTime @x (IO/Done x))
sleep hi_lo = (IO/Sleep hi_lo @x (IO/Done x))
