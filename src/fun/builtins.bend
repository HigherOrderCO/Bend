type String:
  Nil
  Cons { head: u24, ~tail: String }

type List(T):
  Nil
  Cons { head: T, ~tail: List(T) }

# Returns the length of a list and the list itself.
def List/length(xs: List(T)) -> (u24, List(T)):
  fold xs with len=0, acc=DiffList/new:
    case List/Nil:
      return (len, DiffList/to_list(acc))
    case List/Cons:
      return xs.tail(len + 1, DiffList/append(acc, xs.head))

# Reverses a list.
def List/reverse(xs: List(T)) -> List(T):
  fold xs with acc=[]:
    case List/Nil:
      return acc
    case List/Cons:
      return xs.tail(List/Cons(xs.head, acc))

# Flattens a list of lists.
List/flatten (xs: (List (List T))) : (List T)
List/flatten (List/Cons x xs) = (List/concat x (List/flatten xs))
List/flatten (List/Nil)       = (List/Nil)

# Concatenates two lists.
List/concat(xs: (List T)) (ys: (List T)) : (List T)
List/concat (List/Cons x xs) ys = (List/Cons x (List/concat xs ys))
List/concat (List/Nil)       ys = ys

# Splits a list into two lists at the first occurrence of a value.
# Returns the original list if the value is not found
def List/split_once(xs: List(T), val: T) -> (Result((List(T), List(T)), List(T))):
  return List/split_once.go(xs, val, DiffList/new)

def List/split_once.go(
  xs: List(T),
  val: T,
  acc: List(T) -> List(T)
) -> Result((List(T), List(T)), List(T)):
  match xs:
    case List/Nil:
      return Result/Err(DiffList/to_list(acc))
    case List/Cons:
      if val == xs.head:
        return Result/Ok((DiffList/to_list(acc), xs.tail))
      else:
        return List/split_once.go(xs.tail, val, DiffList/append(acc, xs.head))

# Filters a list based on a predicate function.
List/filter (xs: (List T)) (pred: T -> Bool) : (List T)
List/filter (List/Nil)       _    = List/Nil
List/filter (List/Cons x xs) pred =
  if (pred x) {
    (List/Cons x (List/filter xs pred))
  } else {
    (List/filter xs pred)
  }

# Checks if two strings are equal.
String/equals (s1: String) (s2: String) : u24
String/equals (String/Nil) (String/Nil) = 1
String/equals (String/Cons x xs) (String/Cons y ys) =
  if (== x y) {
    (String/equals xs ys)
  } else {
    0
  }
String/equals * * = 0

# Splits a list into two lists at the first occurrence of a value.
String/split (s: String) (delimiter: u24) : (List String)
String/split s delim = (String/split.go s delim (List/Cons String/Nil List/Nil))
  String/split.go (String/Nil) _ acc = (List/reverse acc)
  String/split.go (String/Cons c cs) delim acc =
    if (== c delim) {
      (String/split.go cs delim (List/Cons String/Nil acc))
    } else {
      match acc {
        List/Cons: (String/split.go cs delim (List/Cons (String/Cons c acc.head) acc.tail))
        List/Nil: []
      }
    }

# Create a new difference list
def DiffList/new() -> (List(T) -> List(T)):
  return lambda x: x

# Creates a new difference list with just the given value.
def DiffList/wrap(head: T) -> (List(T) -> List(T)):
  return lambda tail: List/Cons(head, tail)

# Append a value to the end of the difference list
def DiffList/append(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T)):
  return lambda x: diff(List/Cons(val, x))

# Concatenates two difference lists.
def DiffList/concat(
  left: List(T) -> List(T),
  right: List(T) -> List(T)
) -> (List(T) -> List(T)):
  return lambda x: left(right(x))

# Append a value to the beginning of the difference list
def DiffList/cons(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T)):
  return lambda x: List/Cons(val, diff(x))

# Convert a difference list to a list
def DiffList/to_list(diff: List(T) -> List(T)) -> (List(T)):
  return diff(List/Nil)

type Nat = (Succ ~(pred: Nat)) | (Zero)

type (Result o e) = (Ok (val: o)) | (Err (val: e))

def Result/unwrap(res: Result(T, E)) -> Any:
  match res:
    case Result/Ok:
      return res.val
    case Result/Err:
      return res.val

type Tree(T):
  Node { ~left: Tree(T), ~right: Tree(T) }
  Leaf { value: T }

# Returns a List converted from a Tree.
def Tree/to_list(tree):
  fold tree:
    case Tree/Leaf:
      list = DiffList/wrap(tree.value)
    case Tree/Node:
      list = DiffList/concat(tree.left, tree.right)
  return DiffList/to_list(list)

# Reverses a tree swapping right and left leaves.
def Tree/reverse(tree):
  fold tree:
    case Tree/Leaf:
      return !tree.value
    case Tree/Node:
      return ![tree.right, tree.left]

# MAP Impl

type Map T = (Node (value: T) ~(left: (Map T)) ~(right: (Map T))) | (Leaf)

Map/empty : (Map T) = Map/Leaf

Map/get (map: (Map T)) (key: u24) : (T, (Map T)) =
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

Map/set (map: (Map T)) (key: u24) (value: T) : (Map T) =
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

Map/map (map: (Map T)) (key: u24) (f: T -> T) : (Map T)
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

type IO(T):
  Done { magic: (u24, u24), expr: T }
  Call { magic: (u24, u24), func: String, argm: Any, cont: Any -> IO(T) }

type IOError(T):
  Type
  Name
  Inner { value: T }

def IO/MAGIC() -> (u24, u24):
  return (0xD0CA11, 0xFF1FF1)

def IO/wrap(x: T) -> IO(T):
  return IO/Done(IO/MAGIC, x)

def IO/bind(a: IO(A), b: ((A -> IO(B)) -> (A -> IO(B))) -> (A -> IO(B))) -> IO(B):
  match a:
    case IO/Done:
      b = undefer(b)
      return b(a.expr)
    case IO/Call:
      return IO/Call(a.magic, a.func, a.argm, lambda x: IO/bind(a.cont(x), b))

def IO/call(func: String, argm: Any) -> IO(Result(Any, Any)):
  return IO/Call(IO/MAGIC, func, argm, lambda x: IO/Done(IO/MAGIC, x))

IO/done_on_err (IO/Call magic func argm cont) = (IO/Call magic func argm @res match res {
  Result/Ok: (cont res.val)
  Result/Err: (IO/Done IO/MAGIC (Result/Err res.val))
})
IO/done_on_err done = done

## Time and sleep
# Returns a monotonically increasing nanosecond timestamp as an u48 encoded as a pair of u24s.
def IO/get_time() -> IO((u24, u24)):
  with IO:
    res <- IO/call("GET_TIME", *)
    return wrap(Result/unwrap(res))

# Sleeps for the given number of nanoseconds, given by an u48 encoded as a pair of u24s.
def IO/nanosleep(hi_lo: (u24, u24)) -> IO(None):
  return IO/call("SLEEP", hi_lo)

# Sleeps for a given amount of seconds as a float.
def IO/sleep(seconds: f24) -> IO(None):
  nanos = seconds * 1_000_000_000.0
  lo = f24_to_u24(nanos % 0x1_000_000.0)
  hi = f24_to_u24(nanos / 0x1_000_000.0)
  with IO:
    res <- IO/nanosleep((hi, lo))
    return wrap(Result/unwrap(res))

## File IO

### File IO primitives
def IO/FS/open(path: String, mode: String) -> IO(u24):
  return IO/call("OPEN", (path, mode))

def IO/FS/close(file: u24) -> IO(None):
  return IO/call("CLOSE", file)

def IO/FS/read(file: u24, num_bytes: u24) -> IO(List(u24)):
  return IO/call("READ", (file, num_bytes))

def IO/FS/write(file: u24, bytes: List(u24)) -> IO(None):
  return IO/call("WRITE", (file, bytes))

def IO/FS/seek(file: u24, offset: i24, mode: u24) -> IO(None):
  return IO/call("SEEK", (file, (offset, mode)))

def IO/FS/flush(file: u24) -> IO(None):
  return IO/call("FLUSH", file)

### Always available files
IO/FS/STDIN : u24 = 0
IO/FS/STDOUT : u24 = 1
IO/FS/STDERR : u24 = 2

### Seek modes
# Seek from start of file.
IO/FS/SEEK_SET : i24 = +0
# Seek from current position.
IO/FS/SEEK_CUR : i24 = +1
# Seek from end of file.
IO/FS/SEEK_END : i24 = +2

### File utilities

# Reads an entire file, returning a list of bytes.
def IO/FS/read_file(path: String) -> IO(List(u24)):
  with IO:
    fd <- IO/done_on_err(IO/FS/open(path, "r"))
    bytes <- IO/FS/read_to_end(fd)
    * <- IO/done_on_err(IO/FS/close(fd))
    return wrap(bytes)

# Reads the remaining contents of a file, returning a list of read bytes.
def IO/FS/read_to_end(fd: u24) -> IO(List(u24)):
  return IO/FS/read_to_end.read_chunks(fd, [])

def IO/FS/read_to_end.read_chunks(fd: u24, chunks: List(List(u24))) -> IO(List(u24)):
  with IO:
    # Read file in 1MB chunks
    chunk <- IO/done_on_err(IO/FS/read(fd, 1048576))
    match chunk:
      case List/Nil:
        return wrap(List/flatten(chunks))
      case List/Cons:
        return IO/FS/read_to_end.read_chunks(fd, List/Cons(chunk, chunks))

# Reads a single line from a file, returning a list of bytes.
def IO/FS/read_line(fd: u24) -> IO(List(u24)):
  return IO/FS/read_line.read_chunks(fd, [])

def IO/FS/read_line.read_chunks(fd: u24, chunks: List(List(u24))) -> IO(List(u24)):
  with IO:
    # Read line in 1kB chunks
    chunk <- IO/done_on_err(IO/FS/read(fd, 1024))
    match res = List/split_once(chunk, '\n'):
      # Found a newline, backtrack and join chunks
      case Result/Ok:
        (line, rest) = res.val
        (length, *) = List/length(rest)
        * <- IO/FS/seek(fd, u24_to_i24(length) * -1, IO/FS/SEEK_CUR)
        chunks = List/Cons(line, chunks)
        bytes = List/flatten(chunks)
        return wrap(bytes)
      # Newline not found
      case Result/Err:
        line = res.val
        (length, line) = List/length(line)
        # If length is 0, the end of the file was reached, return as if it was a newline
        if length == 0:
          bytes = List/flatten(chunks)
          return wrap(bytes)
        # Otherwise, the line is still ongoing, read more chunks
        else:
          chunks = List/Cons(line, chunks)
          return IO/FS/read_line.read_chunks(fd, chunks)

# Writes a list of bytes to a file given by a path.
def IO/FS/write_file(path: String, bytes: List(u24)) -> IO(None):
  with IO:
    f <- IO/FS/open(path, "w")
    match f:
      case Result/Err:
        return wrap(Result/Err(f.val))
      case Result/Ok:
        * <- IO/done_on_err(IO/FS/write(f.val, bytes))
        * <- IO/done_on_err(IO/FS/close(f.val))
        return wrap(Result/Ok(bytes))

### Standard input and output utilities

# Prints a string to stdout, encoding it with utf-8.
def IO/print(text: String) -> IO(None):
  with IO:
    res <- IO/FS/write(IO/FS/STDOUT, String/encode_utf8(text))
    return wrap(Result/unwrap(res))

# IO/input() -> IO String
# Read characters from stdin until a newline is found.
# Returns the read input decoded as utf-8.
def IO/input() -> IO(String):
  return IO/input.go(DiffList/new)

def IO/input.go(acc: List(u24) -> List(u24)) -> IO(String):
  # TODO: This is slow and inefficient, should be done in hvm using fgets.
  with IO:
    byte <- IO/done_on_err(IO/FS/read(IO/FS/STDIN, 1))
    match byte:
      case List/Nil:
        # Nothing read, try again (to simulate blocking a read)
        return IO/input.go(acc)
      case List/Cons:
        if byte.head == '\n':
          bytes = DiffList/to_list(acc)
          text = String/decode_utf8(bytes)
          return wrap(text)
        else:
          acc = DiffList/append(acc, byte.head)
          return IO/input.go(acc)

### Dynamically linked libraries

# Returns an unique id to the library object encoded as a u24
# 'path' is the path to the library file.
# 'lazy' is a boolean encoded as a u24 that determines if all functions are loaded lazily or upfront.
def IO/DyLib/open(path: String, lazy: u24) -> IO(u24):
  return IO/call("DL_OPEN", (path, lazy))

# Calls a function of a previously opened library.
# The returned value is determined by the called function.
# 'dl' is the id of the library object.
# 'fn' is the name of the function in the library.
# 'args' are the arguments to the function. The expected values depend on the called function.
def IO/DyLib/call(dl: u24, fn: String, args: Any) -> IO(Any):
  return IO/call("DL_CALL", (dl, (fn, args)))

# Closes a previously open library.
# Returns nothing.
# 'dl' is the id of the library object.
def IO/DyLib/close(dl: u24) -> IO(None):
  return IO/Call(IO/MAGIC, "DL_CLOSE", (dl), IO/wrap)

# Lazy thunks
# We can defer the evaluation of a function by wrapping it in a thunk
# Ex: @x (x @arg1 @arg2 @arg3 (f arg1 arg2 arg3) arg1 arg2 arg3)
# This is only evaluated when we call it with ' ' (undefer my_thunk)
# We can build a defered call directly or by by using defer and defer_arg
# The example above can be written as:
# (defer_arg (defer_arg (defer_arg (defer @arg1 @arg2 @arg3 (f arg1 arg2 arg3)) arg1) arg2) arg3)
def defer(val: T) -> (T -> T) -> T:
  return lambda x: x(val)

def defer_arg(defered: ((A -> B) -> (A -> B)) -> (A -> B), arg: B) -> ((A -> A) -> A):
  return lambda x: defered(x, arg)

def undefer(defered: ((A -> A) -> A)) -> A:
  return defered(lambda x: x)


# Native number casts

# f24_to_u24(x: f24) -> u24
# Casts a f24 number to a u24.
hvm f24_to_u24 -> (f24 -> u24):
  ($([u24] ret) ret)

# i24_to_u24(x: i24) -> u24
# Casts an i24 number to a u24.
hvm i24_to_u24 -> (i24 -> u24):
  ($([u24] ret) ret)

# u24_to_i24(x: u24) -> i24
# Casts a u24 number to an i24.
hvm u24_to_i24 -> (u24 -> i24):
  ($([i24] ret) ret)

# f24_to_i24(x: f24) -> i24
# Casts a f24 number to an i24.
hvm f24_to_i24 -> (f24 -> i24):
  ($([i24] ret) ret)

# u24_to_f24(x: u24) -> f24
# Casts a u24 number to a f24.
hvm u24_to_f24 -> (u24 -> f24):
  ($([f24] ret) ret)

# i24_to_f24(x: i24) -> f24
# Casts an i24 number to a f24.
hvm i24_to_f24 -> (i24 -> f24):
  ($([f24] ret) ret)


# String Encoding and Decoding

Utf8/REPLACEMENT_CHARACTER : u24 = '\u{FFFD}'

# String/decode_utf8(List u24) -> String
# Decodes a sequence of bytes to a String using utf-8 encoding.
String/decode_utf8 (bytes: (List u24)) : String =
  let (got, rest) = (Utf8/decode_character bytes)
  match rest {
    List/Nil: (String/Cons got String/Nil)
    List/Cons: (String/Cons got (String/decode_utf8 rest))
  }

# Decodes one utf-8 character from the start of a sequence of bytes.
# Returns the decoded character and the remaining bytes.
Utf8/decode_character (bytes: (List u24)) : (u24, (List u24))
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

# Encodes a string to a sequence of bytes using utf-8 encoding.
String/encode_utf8 (str: String) : (List u24)
String/encode_utf8 (String/Nil)       = (List/Nil)
String/encode_utf8 (String/Cons x xs) =
  use Utf8/rune1max = 0b01111111
  use Utf8/rune2max = 0b00000111_11111111
  use Utf8/rune3max = 0b11111111_11111111
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

# Decodes a sequence of bytes to a String using ascii encoding.
String/decode_ascii (bytes: (List u24)) : String
String/decode_ascii (List/Cons x xs) = (String/Cons x (String/decode_ascii xs))
String/decode_ascii (List/Nil)       = (String/Nil)

# Encodes a string to a sequence of bytes using ascii encoding.
String/encode_ascii (str: String) : (List u24)
String/encode_ascii (String/Cons x xs) = (List/Cons x (String/encode_ascii xs))
String/encode_ascii (String/Nil)       = (List/Nil)

# Math

# Math/PI() -> f24
# The Pi (π) constant.
def Math/PI() -> f24:
  return 3.1415926535

def Math/E() -> f24:
  return 2.718281828

# Math/log(x: f24, base: f24) -> f24
# Computes the logarithm of `x` with the specified `base`.
hvm Math/log -> (f24 -> f24 -> f24):
  (x ($([|] $(x ret)) ret))

# Math/atan2(x: f24, y: f24) -> f24
# Has the same behaviour as `atan2f` in the C math lib.
# Computes the arctangent of the quotient of its two arguments.
hvm Math/atan2 -> (f24 -> f24 -> f24):
  ($([&] $(y ret)) (y ret))


# Math/sin(a: f24) -> f24
# Computes the sine of the given angle in radians.
hvm Math/sin -> (f24 -> f24):
  ($([<<0x0] a) a)

# Math/cos(a: f24) -> f24
# Computes the cosine of the given angle in radians.
hvm Math/cos -> (f24 -> f24):
  (a b)
  & @Math/PI ~ $([:/2.0] $([-] $(a $([<<0x0] b))))

# Math/tan(a: f24) -> f24
# Computes the tangent of the given angle in radians.
hvm Math/tan -> (f24 -> f24):
  ($([>>0x0] a) a)

# Computes the cotangent of the given angle in radians.
Math/cot (a: f24) : f24 =
  (/ 1.0 (Math/tan a))

# Computes the secant of the given angle in radians.
Math/sec (a: f24) : f24 =
  (/ 1.0 (Math/cos a))

# Computes the cosecant of the given angle in radians.
Math/csc (a: f24) : f24 =
  (/ 1.0 (Math/sin a))

# Computes the arctangent of the given angle.
Math/atan (a: f24) : f24 =
  (Math/atan2 a 1.0)

# Computes the arcsine of the given angle.
Math/asin (a: f24) : f24 =
  (Math/atan2 a (Math/sqrt (- 1.0 (* a a))))

# Computes the arccosine of the given angle.
Math/acos (a: f24) : f24 =
  (Math/atan2 (Math/sqrt (- 1.0 (* a a))) a)

# Converts degrees to radians.
Math/radians (a: f24) : f24 =
  (* a (/ Math/PI 180.0))

# Computes the square root  of the given number.
Math/sqrt (n: f24) : f24 =
  (** n 0.5)

# Round float up to the nearest integer.
def Math/ceil(n: f24) -> f24:
  i_n = i24_to_f24(f24_to_i24(n))
  if n <= i_n:
    return i_n
  else:
    return i_n + 1.0

# Round float down to the nearest integer.
def Math/floor(n: f24) -> f24:
  i_n = i24_to_f24(f24_to_i24(n))
  if n < i_n:
    return i_n - 1.0
  else:
    return i_n

# Round float to the nearest integer.
def Math/round(n: f24) -> f24:
  i_n = i24_to_f24(f24_to_i24(n))
  if (n - i_n) < 0.5:
    return Math/floor(n)
  else:
    return Math/ceil(n)
