type String = (Nil) | (Cons head ~tail)

type List   = (Nil) | (Cons head ~tail)

type Nat    = (Succ ~pred) | (Zero)

type Tree:
  Node { ~left, ~right }
  Leaf { value }


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

Map/map (Map/Leaf)                  key f = Map/Leaf
Map/map (Map/Node value left right) key f =
  switch _ = (== 0 key) {
    0: switch _ = (% key 2) {
      0:
        (Map/Node value (Map/map left (/ key 2) f) right)
      _:
        (Map/Node value left (Map/map right (/ key 2) f))
    }
    _: (Map/Node (f value) left right)
  }


# IO Impl

type IO:
  Done { magic, expr }
  Call { magic, func, argm, cont }

def IO/MAGIC:
  return (0xD0CA11, 0xFF1FF1)

def IO/wrap(x):
  return IO/Done(IO/MAGIC, x)

def IO/bind(a, b):
  match a:
    case IO/Done:
      b = undefer(b)
      return b(a.expr)
    case IO/Call:
      return IO/Call(IO/MAGIC, a.func, a.argm, lambda x: IO/bind(a.cont(x), b))

def call(func, argm):
  return IO/Call(IO/MAGIC, func, argm, lambda x: IO/Done(IO/MAGIC, x))

## Time and sleep
# Returns a monotonically increasing nanosecond timestamp as an u48 encoded as a pair of u24s.
IO/get_time = (IO/Call IO/MAGIC "GET_TIME" * @x (IO/Done IO/MAGIC x))

# Sleeps for the given number of nanoseconds, given by an u48 encoded as a pair of u24s.
IO/nanosleep hi_lo = (IO/Call IO/MAGIC "SLEEP" hi_lo @x (IO/Done IO/MAGIC x))

# Sleeps for a given amount of seconds as a float.
def IO/sleep(seconds):
  nanos = seconds * 1_000_000_000.0
  lo = to_u24(nanos % 0x1_000_000.0)
  hi = to_u24(nanos / 0x1_000_000.0)
  return IO/nanosleep((hi, lo))

## File IO

### File IO primitives
IO/FS/open path mode = (IO/Call IO/MAGIC "OPEN" (path, mode) @x (IO/Done IO/MAGIC x))
IO/FS/close file = (IO/Call IO/MAGIC "CLOSE" file @x (IO/Done IO/MAGIC x))
IO/FS/read file num_bytes = (IO/Call IO/MAGIC "READ" (file, num_bytes) @x (IO/Done IO/MAGIC x))
IO/FS/write file bytes = (IO/Call IO/MAGIC "WRITE" (file, bytes) @x (IO/Done IO/MAGIC x))
IO/FS/seek file offset mode = (IO/Call IO/MAGIC "SEEK" (file, (offset, mode)) @x (IO/Done IO/MAGIC x))

### Always available files
IO/FS/STDIN = 0
IO/FS/STDOUT = 1
IO/FS/STDERR = 2

### Seek modes
# Seek from start of file
IO/FS/SEEK_SET = +0
# Seek from current position
IO/FS/SEEK_CUR = +1
# Seek from end of file
IO/FS/SEEK_END = +2

### File utilities
#read_file path = (IO/Call IO/MAGIC "GET_FILE" path @x (IO/Done IO/MAGIC x))
#write_file path text = (IO/Call IO/MAGIC "PUT_FILE" (path, text) @x (IO/Done IO/MAGIC x))

### Standard input and output utilities

# TODO: Encode with utf-8
IO/print text = (IO/FS/write IO/FS/STDOUT text)

# TODO: Should read any length of input
IO/input = (IO/FS/read IO/FS/STDIN 1024)

# Lazy thunks
# We can defer the evaluation of a function by wrapping it in a thunk
# Ex: @x (x @arg1 @arg2 @arg3 (f arg1 arg2 arg3) arg1 arg2 arg3)
# This is only evaluated when we call it with 'undefer' (undefer my_thunk)
# We can build a defered call directly or by by using defer and defer_arg
# The example above can be written as:
# (defer_arg (defer_arg (defer_arg (defer @arg1 @arg2 @arg3 (f arg1 arg2 arg3)) arg1) arg2) arg3)
defer val = @x (x val)
defer_arg defered arg = @x (defered x arg)
undefer defered = (defered @x x)


# Native numeric operations

# log(x: f24, base: f24) -> f24
# Computes the logarithm of `x` with the specified `base`.
hvm log:
  (x ($([|] $(x ret)) ret))

# atan2(x: f24, y: f24) -> f24
# Has the same behaviour as `atan2f` in the C math lib.
# Computes the arctangent of the quotient of its two arguments.
hvm atan2:
  ($([&] $(y ret)) (y ret))

# to_f24(x: native number) -> f24
# Casts any native number to an f24.
hvm to_f24:
  ($([f24] ret) ret)

# to_u24(x: native number) -> u24
# Casts any native number to a u24.
hvm to_u24:
  ($([u24] ret) ret)

# to_i24(x: native number) -> i24
# Casts any native number to an i24.
hvm to_i24:
  ($([i24] ret) ret)

# Encoding

Utf8/REPLACEMENT_CHARACTER = '\u{FFFD}'

Bytes/decode_utf8 bytes =
  let (got, rest) = (Utf8/decode_character bytes)
  match rest {
    List/Nil: (String/Cons got String/Nil)
    List/Cons: (String/Cons got (Bytes/decode_utf8 rest))
  }

Utf8/decode_character [] = (0, [])
Utf8/decode_character [a] = if (<= a 0x7F) { (a, []) } else { (Utf8/REPLACEMENT_CHARACTER, []) }
Utf8/decode_character [a, b] =
  use Utf8/maskx = 0b00111111
  use Utf8/mask2 = 0b00011111
  if (<= a 0x7F) {
    (a, [b])
  } else {
    if (== (& a 0xE0) 0xC0) {
      let r = (| (<< (& a Utf8/mask2) 6) (& b Utf8/maskx))
      (r, [])
    } else {
      (Utf8/REPLACEMENT_CHARACTER, [])
    }
  }
Utf8/decode_character [a, b, c] =
  use Utf8/maskx = 0b00111111
  use Utf8/mask2 = 0b00011111
  use Utf8/mask3 = 0b00001111
  if (<= a 0x7F) {
    (a, [b, c])
  } else {
    if (== (& a 0xE0) 0xC0) {
      let r = (| (<< (& a Utf8/mask2) 6) (& b Utf8/maskx))
      (r, [c])
    } else {
      if (== (& a 0xF0) 0xE0) {
        let r = (| (<< (& a Utf8/mask3) 12) (| (<< (& b Utf8/maskx) 6) (& c Utf8/maskx)))
        (r, [])
      } else {
        (Utf8/REPLACEMENT_CHARACTER, [])
      }
    }
  }
Utf8/decode_character (List/Cons a (List/Cons b (List/Cons c (List/Cons d rest)))) =
  use Utf8/maskx = 0b00111111
  use Utf8/mask2 = 0b00011111
  use Utf8/mask3 = 0b00001111
  use Utf8/mask4 = 0b00000111
  if (<= a 0x7F) {
    (a, (List/Cons b (List/Cons c (List/Cons d rest))))
  } else {
    if (== (& a 0xE0) 0xC0) {
      let r = (| (<< (& a Utf8/mask2) 6) (& b Utf8/maskx))
      (r, (List/Cons c (List/Cons d rest)))
    } else {
      if (== (& a 0xF0) 0xE0) {
        let r = (| (<< (& a Utf8/mask3) 12) (| (<< (& b Utf8/maskx) 6) (& c Utf8/maskx)))
        (r, (List/Cons d rest))
      } else {
        if (== (& a 0xF8) 0xF0) {
          let r = (| (<< (& a Utf8/mask4) 18) (| (<< (& b Utf8/maskx) 12) (| (<< (& c Utf8/maskx) 6) (& d Utf8/maskx))))
          (r, [])
        } else {
          (Utf8/REPLACEMENT_CHARACTER, rest)
        }
      }
    }
  }

String/encode_utf8 (String/Nil)       = (List/Nil)
String/encode_utf8 (String/Cons x xs) =
  use Utf8/rune1max = (- (<< 1 7) 1)
  use Utf8/rune2max = (- (<< 1 11) 1)
  use Utf8/rune3max = (- (<< 1 16) 1)
  use Utf8/tx       = 0b10000000
  use Utf8/t2       = 0b11000000
  use Utf8/t3       = 0b11100000
  use Utf8/t4       = 0b11110000
  use Utf8/maskx    = 0b00111111
  if (<= x Utf8/rune1max) {
    (List/Cons x (String/encode_utf8 xs))
  } else {
    if (<= x Utf8/rune2max) {
      let b1 = (| Utf8/t2 (>> x 6))
      let b2 = (| Utf8/tx (& x Utf8/maskx))
      (List/Cons b1 (List/Cons b2 (String/encode_utf8 xs)))
    } else {
      if (<= x Utf8/rune3max) {
        let b1 = (| Utf8/t3 (>> x 12))
        let b2 = (| Utf8/tx (& (>> x 6) Utf8/maskx))
        let b3 = (| Utf8/tx (& x        Utf8/maskx))
        (List/Cons b1 (List/Cons b2 (List/Cons b3 (String/encode_utf8 xs))))
      } else {
        let b1 = (| Utf8/t4 (>> x 18))
        let b2 = (| Utf8/tx (& (>> x 12) Utf8/maskx))
        let b3 = (| Utf8/tx (& (>> x 6)  Utf8/maskx))
        let b4 = (| Utf8/tx (& x         Utf8/maskx))
        (List/Cons b1 (List/Cons b2 (List/Cons b3 (List/Cons b4 (String/encode_utf8 xs)))))
      }
    }
  }

Bytes/decode_ascii (List/Cons x xs) = (String/Cons x (Bytes/decode_ascii xs))
Bytes/decode_ascii (List/Nil)       = (String/Nil)

String/encode_ascii (String/Cons x xs) = (List/Cons x (String/encode_ascii xs))
String/encode_ascii (String/Nil)       = (List/Nil)
