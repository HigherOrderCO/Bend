type String:
  Nil
  Cons { head: u24, ~tail: String }

type List(T):
  Nil
  Cons { head: T, ~tail: List(T) }

#{ Returns a tuple containing the length and the list itself. #}
def List/length(xs: List(T)) -> (u24, List(T)):
  fold xs with len=0, acc=DiffList/new:
    case List/Nil:
      return (len, DiffList/to_list(acc))
    case List/Cons:
      return xs.tail(len + 1, DiffList/append(acc, xs.head))

#{ Reverses the elements of a list. #}
def List/reverse(xs: List(T)) -> List(T):
  fold xs with acc=[]:
    case List/Nil:
      return acc
    case List/Cons:
      return xs.tail(List/Cons(xs.head, acc))

#{ Returns a flattened list from a list of lists. #}
List/flatten (xs: (List (List T))) : (List T)
List/flatten (List/Cons x xs) = (List/concat x (List/flatten xs))
List/flatten (List/Nil)       = (List/Nil)

#{ Appends two lists together. #}
List/concat(xs: (List T)) (ys: (List T)) : (List T)
List/concat (List/Cons x xs) ys = (List/Cons x (List/concat xs ys))
List/concat (List/Nil)       ys = ys

#{
  Splits a list into two lists at the first occurrence of a value.
#}
def List/split_once(
  xs: List(T),
  cond: T -> u24
) -> (Result((List(T), List(T)), List(T))):
  return List/split_once.go(xs, cond, DiffList/new)

def List/split_once.go(
  xs: List(T),
  cond: T -> u24,
  acc: List(T) -> List(T)
) -> Result((List(T), List(T)), List(T)):
  match xs:
    case List/Nil:
      return Result/Err(DiffList/to_list(acc))
    case List/Cons:
      if cond(xs.head):
        return Result/Ok((DiffList/to_list(acc), xs.tail))
      else:
        return List/split_once.go(xs.tail, cond, DiffList/append(acc, xs.head))

#{ Filters a list based on a predicate function. #}
List/filter (xs: (List T)) (pred: T -> u24) : (List T)
List/filter (List/Nil)       _    = List/Nil
List/filter (List/Cons x xs) pred =
  if (pred x) {
    (List/Cons x (List/filter xs pred))
  } else {
    (List/filter xs pred)
  }

#{ Checks if two strings are equal. #}
String/equals (s1: String) (s2: String) : u24
String/equals (String/Nil) (String/Nil) = 1
String/equals (String/Cons x xs) (String/Cons y ys) =
  if (== x y) {
    (String/equals xs ys)
  } else {
    0
  }
String/equals * * = 0

#{ Splits a string into a list of strings based on the given delimiter. #}
String/split (s: String) (delimiter: u24) : (List String)
String/split s delim = (String/split.go s delim [""])

String/split.go (cs: String) (delim: u24) (acc: (List String)) : (List String)
String/split.go (String/Nil) _ acc = (List/reverse acc)
String/split.go (String/Cons c cs) delim acc =
  if (== c delim) {
#{ Start a new split string #}
    (String/split.go cs delim (List/Cons String/Nil acc))
  } else {
    match acc {
#{ Add the current character to the current split string #}
      List/Cons: (String/split.go cs delim (List/Cons (String/Cons c acc.head) acc.tail))
#{ Should be unreachable #}
      List/Nil: []
    }
  }

#{ Create a new difference list #}
def DiffList/new() -> (List(T) -> List(T)):
  return lambda x: x

#{ Creates a new difference list with just the given value. #}
def DiffList/wrap(head: T) -> (List(T) -> List(T)):
  return lambda tail: List/Cons(head, tail)

#{ Append a value to the end of the difference list #}
def DiffList/append(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T)):
  return lambda x: diff(List/Cons(val, x))

#{ Concatenates two difference lists. #}
def DiffList/concat(
  left: List(T) -> List(T),
  right: List(T) -> List(T)
) -> (List(T) -> List(T)):
  return lambda x: left(right(x))

#{ Append a value to the beginning of the difference list #}
def DiffList/cons(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T)):
  return lambda x: List/Cons(val, diff(x))

#{ Converts a difference list to a regular cons list. #}
def DiffList/to_list(diff: List(T) -> List(T)) -> (List(T)):
  return diff(List/Nil)

type Nat = (Succ ~(pred: Nat)) | (Zero)

type (Result o e) = (Ok (val: o)) | (Err (val: e))

#{
Returns the inner value of `Result/Ok` or `Result/Err`.

If the types `A` and `B` are different, should only be used in type unsafe programs or when only one variant is guaranteed to happen.
#}
def Result/unwrap(res: Result(T, E)) -> Any:
  match res:
    case Result/Ok:
      return res.val
    case Result/Err:
      return res.val

#{
  Returns the second result if the first one is `Ok`.
  Otherwise, returns the `Err` of the first result.
#}
def Result/and(fst: Result(A, E), snd: Result(B, E)) -> Result(B, E):
  match fst:
    case Result/Ok:
      return snd
    case Result/Err:
      return fst

#{ Maps the error value of a result. #}
def Result/map_err(res: Result(T, E), f: E -> F) -> Result(T, F):
  match res:
    case Result/Ok:
      return Result/Ok(res.val)
    case Result/Err:
      return Result/Err(f(res.val))

type Tree(T):
  Node { ~left: Tree(T), ~right: Tree(T) }
  Leaf { value: T }

#{ Returns a List converted from a Tree. #}
def Tree/to_list(tree: Tree(T)) -> List(T):
  fold tree:
    case Tree/Leaf:
      list = DiffList/wrap(tree.value)
    case Tree/Node:
      list = DiffList/concat(tree.left, tree.right)
  return DiffList/to_list(list)

#{ Reverses a tree swapping right and left leaves. #}
def Tree/reverse(tree: Tree(T)) -> Tree(T):
  fold tree:
    case Tree/Leaf:
      return !tree.value
    case Tree/Node:
      return ![tree.right, tree.left]

#{ MAYBE Impl #}

type Maybe(T):
  Some { value: T }
  None 

#{ Returns the value inside the `Maybe` if it is `Some`, and returns `unreachable()` if it is `None`. #}  
def Maybe/unwrap(m: Maybe(T)) -> T:
  match m:
    case Maybe/Some:
      return m.value
    case Maybe/None:
      return unreachable()

#{ MAP Impl #}

type Map(T):
  Node { value: Maybe(T), ~left: Map(T), ~right: Map(T) }
  Leaf  

#{ Initializes an empty map. #}
def Map/empty() -> Map(T):
  return Map/Leaf

#{
  Retrieves a `value` from the `map` based on the `key` and returns a tuple with the value and the `map` unchanged. 
  The logic for checking whether a value is or not contained in a `map` is not done in the `get` function, so if 
  we try to get a key that is not in the map, the program will return `unreachable`. 
#}
def Map/get (map: Map(T), key: u24) -> (T, Map(T)):
  match map:
    case Map/Leaf:
      return (unreachable(), map)
    case Map/Node:
      if (0 == key):
        return (Maybe/unwrap(map.value), map)
      elif (key % 2 == 0):
        (got, rest) = Map/get(map.left, (key / 2))
        return(got, Map/Node(map.value, rest, map.right))
      else:
        (got, rest) = Map/get(map.right, (key / 2))
        return(got, Map/Node(map.value, map.left, rest))


#{ Checks if a node has a value on a given key, returning Maybe/Some if it does, Maybe/None otherwise #}
def Map/get_check (map: Map(T), key: u24) -> (Maybe(T), Map(T)):
  match map:
    case Map/Leaf:
      return (Maybe/None, map)
    case Map/Node:
      if (0 == key):
        return (map.value, map)
      elif (key % 2 == 0):
        (new_value, new_map) = Map/get_check(map.left, (key / 2))
        return (new_value, Map/Node(map.value, new_map, map.right))
      else:
        (new_value, new_map) = Map/get_check(map.right, (key / 2))
        return (new_value, Map/Node(map.value, map.left, new_map))

#{ Sets a value on a Map, returning the map with the value mapped. #}
def Map/set (map: Map(T), key: u24, value: T) -> Map(T):
  match map:
    case Map/Node:
      if (0 == key):
        return Map/Node(Maybe/Some(value), map.left, map.right)
      elif ((key % 2) == 0):
        return Map/Node(map.value, Map/set(map.left, (key / 2), value), map.right)
      else:
        return Map/Node(map.value, map.left, Map/set(map.right, (key / 2), value))
    case Map/Leaf:
      if (0 == key):
        return Map/Node(Maybe/Some(value), Map/Leaf, Map/Leaf)
      elif ((key % 2) == 0):
        return Map/Node(Maybe/None, Map/set(Map/Leaf, (key / 2), value), Map/Leaf)
      else:
        return Map/Node(Maybe/None, Map/Leaf, Map/set(Map/Leaf, (key / 2),value))


#{ Checks if a `map` contains a given `key` and returns 0 or 1 along with and  `map` unchanged. #}
def Map/contains (map: Map(T), key: u24) -> (u24, Map(T)):
  match map:
    case Map/Leaf:
      return (0, map)
    case Map/Node:
      if (0 == key):
        match map.value:
          case Maybe/Some:
            return (1, map)
          case Maybe/None:
            return (0, map)
      elif ((key % 2) == 0):
        (new_value, new_map) = Map/contains(map.left, (key / 2))
        return (new_value, Map/Node(map.value, new_map, map.right))
      else:
        (new_value, new_map) = Map/contains(map.right, (key / 2))
        return (new_value, Map/Node(map.value, map.left, new_map))

#{ Applies a function to a value in the map and returns the map with the value mapped. #}
def Map/map (map: Map(T), key: u24, f: T -> T) -> Map(T):
  match map:
    case Map/Leaf:
      return Map/Leaf
    case Map/Node:
      if (0 == key):
        return Map/Node(Maybe/Some(f(Maybe/unwrap(map.value))), map.left, map.right)
      elif ((key % 2) == 0):
        return Map/Node(map.value, Map/map(map.left, (key / 2), f), map.right)
      else:
        return Map/Node(map.value, map.left, Map/map(map.right, (key / 2), f))

#{ IO Impl #}

type IO(T):
  Done { magic: (u24, u24), expr: T }
  Call { magic: (u24, u24), func: String, argm: Any, cont: Result(Any, IOError(Any)) -> IO(T) }

type IOError(T):
  Type
  Name
  Inner { value: T }

def IOError/unwrap_inner(err: IOError(T)) -> T:
  match err:
    case IOError/Type:
      return unreachable()
    case IOError/Name:
      return unreachable()
    case IOError/Inner:
      return err.value

def IO/MAGIC() -> (u24, u24):
  return (0xD0CA11, 0xFF1FF1)

def IO/wrap(x: T) -> IO(T):
  return IO/Done(IO/MAGIC, x)

def IO/bind(a: IO(A), b: ((Id -> Id) -> A -> IO(B))) -> IO(B):
  match a:
    case IO/Done:
      return undefer(b, a.expr)
    case IO/Call:
      return IO/Call(a.magic, a.func, a.argm, lambda x: IO/bind(a.cont(x), b))

#{
  Calls an IO by its name with the given arguments.
  
  The arguments are untyped and not checked for type correctness.
  If type safety is desired, this function should be wrapped with
  another that checks the types of the arguments and of the return.
  
  Always returns a `Result` where the error is an `IOError`, a type
  that either contains an internal error of the IO function, like an
  `errno` in the case of FS functions, or a general Bend IO error,
  like a type error if the arguments are invalid or a name error if
  the called IO is not found.
#}
def IO/call(func: String, argm: Any) -> IO(Result(Any, IOError(Any))):
  return IO/Call(IO/MAGIC, func, argm, lambda x: IO/Done(IO/MAGIC, x))

#{ Maps the result of an IO. #}
def IO/map(io: IO(A), f: A -> B) -> IO(B):
  with IO:
    a <- io
    return wrap(f(a))

#{
  Unwraps the `IOError` of the result of an IO, returning the `Inner` variant.
  
  Should only be called if the other `IOError` variants are unreachable.
#}
def IO/unwrap_inner(io: IO(Result(A, IOError(B)))) -> IO(Result(A, B)):
  with IO:
    res <- io
    match res:
      case Result/Ok:
        return wrap(Result/Ok(res.val))
      case Result/Err:
        return wrap(Result/Err(IOError/unwrap_inner(res.val)))

## Time and sleep

#{
  Returns a monotonically increasing nanosecond timestamp as an u48
  encoded as a pair of u24s.
#}
def IO/get_time() -> IO((u24, u24)):
  with IO:
    res <- IO/call("GET_TIME", *)
    return wrap(Result/unwrap(res))

#{ Sleeps for the given number of nanoseconds, given by an u48 encoded as a pair of u24s. #}
def IO/nanosleep(hi_lo: (u24, u24)) -> IO(None):
  with IO:
    res <- IO/call("SLEEP", hi_lo)
    return wrap(Result/unwrap(res))

#{ Sleeps for a given amount of seconds as a float. #}
def IO/sleep(seconds: f24) -> IO(None):
  nanos = seconds * 1_000_000_000.0
  lo = f24/to_u24(nanos % 0x1_000_000.0)
  hi = f24/to_u24(nanos / 0x1_000_000.0)
  return IO/nanosleep((hi, lo))

## File IO

### File IO primitives

#{ Opens a file with with `path` being given as a string and `mode` being a string with the mode to open the file in. The mode should be one of the following: #}
def IO/FS/open(path: String, mode: String) -> IO(Result(u24, u24)):
  return IO/unwrap_inner(IO/call("OPEN", (path, mode)))

#{ Closes the file with the given `file` descriptor. #}
def IO/FS/close(file: u24) -> IO(Result(None, u24)):
  return IO/unwrap_inner(IO/call("CLOSE", file))

#{
Reads `num_bytes` bytes from the file with the given `file` descriptor.
Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read(file: u24, num_bytes: u24) -> IO(Result(List(u24), u24)):
  return IO/unwrap_inner(IO/call("READ", (file, num_bytes)))

#{
  Writes `bytes`, a list of U24 with each element representing a byte, to the file with the given `file` descriptor.
  Returns nothing (`*`).
#}
def IO/FS/write(file: u24, bytes: List(u24)) -> IO(Result(None, u24)):
  return IO/unwrap_inner(IO/call("WRITE", (file, bytes)))

#{ Moves the current position of the file with the given `file` descriptor to the given `offset`, an I24 or U24 number, in bytes. #}
def IO/FS/seek(file: u24, offset: i24, mode: i24) -> IO(Result(None, u24)):
  return IO/unwrap_inner(IO/call("SEEK", (file, (offset, mode))))

#{
  Flushes the file with the given `file` descriptor.
  Returns nothing (`*`).
#}
def IO/FS/flush(file: u24) -> IO(Result(None, u24)):
  return IO/unwrap_inner(IO/call("FLUSH", file))

### Always available files
IO/FS/STDIN : u24 = 0
IO/FS/STDOUT : u24 = 1
IO/FS/STDERR : u24 = 2

### Seek modes
#{ Seek from start of file. #}
IO/FS/SEEK_SET : i24 = +0
#{ Seek from current position. #}
IO/FS/SEEK_CUR : i24 = +1
#{ Seek from end of file. #}
IO/FS/SEEK_END : i24 = +2

### File utilities

#{
  Reads an entire file with the given `path` and returns a list of U24 with each 
  element representing a byte read from the file.
#}
def IO/FS/read_file(path: String) -> IO(Result(List(u24), u24)):
  with IO:
    res_fd <- IO/FS/open(path, "r")
    match res_fd:
      case Result/Ok:
        fd = res_fd.val
        res1 <- IO/FS/read_to_end(fd)
        res2 <- IO/FS/close(fd)
        return wrap(Result/and(res2, res1))
      case Result/Err:
        return wrap(Result/Err(res_fd.val))

#{
  Reads until the end of the file with the given `file` descriptor.
  Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read_to_end(fd: u24) -> IO(Result(List(u24), u24)):
  return IO/FS/read_to_end.read_chunks(fd, [])

def IO/FS/read_to_end.read_chunks(fd: u24, chunks: List(List(u24))) -> IO(Result(List(u24), u24)):
  with IO:
    # Read file in 1MB chunks
    res_chunk <- IO/FS/read(fd, 1048576)
    match res_chunk:
      case Result/Ok:
        chunk = res_chunk.val
        match chunk:
          case List/Nil:
            return wrap(Result/Ok(List/flatten(chunks)))
          case List/Cons:
            return IO/FS/read_to_end.read_chunks(fd, List/Cons(chunk, chunks))
      case Result/Err:
        return wrap(Result/Err(res_chunk.val))

#{
  Reads a line from the file with the given `file` descriptor.
  Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read_line(fd: u24) -> IO(Result(List(u24), u24)):
  return IO/FS/read_line.read_chunks(fd, [])

def IO/FS/read_line.read_chunks(fd: u24, chunks: List(List(u24))) -> IO(Result(List(u24), u24)):
  with IO:
    # Read line in 1kB chunks
    res_chunk <- IO/FS/read(fd, 1024)
    match res_chunk:
      case Result/Ok:
        chunk = res_chunk.val
        match res = List/split_once(chunk, lambda x: x == '\n'):
          # Found a newline, backtrack and join chunks
          case Result/Ok:
            (line, rest) = res.val
            (length, *) = List/length(rest)
            res_seek <- IO/FS/seek(fd, u24/to_i24(length) * -1, IO/FS/SEEK_CUR)
            match res_seek:
              case Result/Ok:
                chunks = List/Cons(line, chunks)
                bytes = List/flatten(chunks)
                return wrap(Result/Ok(bytes))
              case Result/Err:
                return wrap(Result/Err(res_seek.val))
          # Newline not found
          case Result/Err:
            line = res.val
            (length, line) = List/length(line)
            # If length is 0, the end of the file was reached, return as if it was a newline
            if length == 0:
              bytes = List/flatten(chunks)
              return wrap(Result/Ok(bytes))
            # Otherwise, the line is still ongoing, read more chunks
            else:
              chunks = List/Cons(line, chunks)
              return IO/FS/read_line.read_chunks(fd, chunks)
      case Result/Err:
        return wrap(Result/Err(res_chunk.val))

#{ Writes `bytes`, a list of U24 with each element representing a byte, as the entire content of the file with the given `path`. #}
def IO/FS/write_file(path: String, bytes: List(u24)) -> IO(Result(None, u24)):
  with IO:
    res_f <- IO/FS/open(path, "w")
    match res_f:
      case Result/Ok:
        f = res_f.val
        res1 <- IO/FS/write(f, bytes)
        res2 <- IO/FS/close(f)
        return wrap(Result/and(res2, res1))
      case Result/Err:
        return wrap(Result/Err(res_f.val))

### Standard input and output utilities

#{ Prints the string `text` to the standard output, encoded with utf-8. #}
def IO/print(text: String) -> IO(None):
  with IO:
    res <- IO/FS/write(IO/FS/STDOUT, String/encode_utf8(text))
    return wrap(Result/unwrap(res))

#{
  IO/input() -> IO String
  Reads characters from the standard input until a newline is found.
  Returns the read input as a String decoded with utf-8.
#}
def IO/input() -> IO(Result(String, u24)):
  return IO/input.go(DiffList/new)

def IO/input.go(acc: List(u24) -> List(u24)) -> IO(Result(String, u24)):
  # TODO: This is slow and inefficient, should be done in hvm using fgets.
  with IO:
    res_byte <- IO/FS/read(IO/FS/STDIN, 1)
    match res_byte:
      case Result/Ok:
        byte = res_byte.val
        match byte:
          case List/Nil:
            # Nothing read, try again (to simulate blocking a read)
            return IO/input.go(acc)
          case List/Cons:
            if byte.head == '\n':
              bytes = DiffList/to_list(acc)
              text = String/decode_utf8(bytes)
              return wrap(Result/Ok(text))
            else:
              acc = DiffList/append(acc, byte.head)
              return IO/input.go(acc)
      case Result/Err:
        return wrap(Result/Err(res_byte.val))

### Dynamically linked libraries

#{
  Loads a dynamic library file.
#}
def IO/DyLib/open(path: String, lazy: u24) -> IO(Result(u24, String)):
  return IO/unwrap_inner(IO/call("DL_OPEN", (path, lazy)))
#{
  - `path` is the path to the library file.
  - `lazy` is a boolean encoded as a `u24` that determines if all functions are loaded lazily (`1`) or upfront (`0`).
  - Returns an unique id to the library object encoded as a `u24`.
#}

#{
  Calls a function of a previously opened library.
  - `dl` is the id of the library object.
  - `fn` is the name of the function in the library.
  - `args` are the arguments to the function. The expected values depend on the called function.
  - The returned value is determined by the called function.
#}
def IO/DyLib/call(dl: u24, fn: String, args: Any) -> IO(Result(Any, String)):
  return IO/unwrap_inner(IO/call("DL_CALL", (dl, (fn, args))))

#{
  Closes a previously open library.
  - `dl` is the id of the library object.
  - Returns nothing (`*`).
#}
def IO/DyLib/close(dl: u24) -> IO(Result(None, String)):
  return IO/unwrap_inner(IO/call("DL_CLOSE", dl))

#{ Lazy thunks #}

#{
  We can defer the evaluation of a function by wrapping it in a thunk.
  Ex: @x (x @arg1 @arg2 @arg3 (f arg1 arg2 arg3) arg1 arg2 arg3)
  
  This is only evaluated when we call it with `(undefer my_thunk)`.
  We can build a defered call directly or by by using `defer` and `defer_arg`.
  
  The example above can be written as:
  
  (defer_arg (defer_arg (defer_arg (defer @arg1 @arg2 @arg3 (f arg1 arg2 arg3)) arg1) arg2) arg3)
#}
def defer(val: T) -> (T -> T) -> T:
  return lambda x: x(val)

def defer_arg(defered: (Id -> Id) -> A -> B, arg: A) -> ((Id -> Id) -> B):
  return lambda x: defered(x, arg)

def undefer(defered: (Id -> Id) -> T) -> T:
  return defered(lambda x: x)

#{
  A function that can be used in unreachable code.
  
  Is not type safe and if used in code that is actually reachable, will corrupt the program.
#}
def unreachable() -> Any:
  return *

#{ Native number casts #}

#{ Casts a f24 number to a u24. #}
hvm f24/to_u24 -> (f24 -> u24):
  ($([u24] ret) ret)

#{ Casts an i24 number to a u24. #}
hvm i24/to_u24 -> (i24 -> u24):
  ($([u24] ret) ret)

#{ Casts a u24 number to an i24. #}
hvm u24/to_i24 -> (u24 -> i24):
  ($([i24] ret) ret)

#{ Casts a f24 number to an i24. #}
hvm f24/to_i24 -> (f24 -> i24):
  ($([i24] ret) ret)

#{ Casts a u24 number to a f24. #}
hvm u24/to_f24 -> (u24 -> f24):
  ($([f24] ret) ret)

#{ Casts an i24 number to a f24. #}
hvm i24/to_f24 -> (i24 -> f24):
  ($([f24] ret) ret)

#{ Casts an u24 native number to a string. #}
def u24/to_string(n: u24) -> String:
  def go(n: u24) -> String -> String:
    r = n % 10
    d = n / 10
    c = '0' + r
    if d == 0:
      return lambda t: String/Cons(c, t)
    else:
      return lambda t: go(d, String/Cons(c, t))
  return go(n, String/Nil)

#{ String Encoding and Decoding #}

Utf8/REPLACEMENT_CHARACTER : u24 = '\u{FFFD}'

#{ Decodes a sequence of bytes to a String using utf-8 encoding. #}
String/decode_utf8 (bytes: (List u24)) : String
String/decode_utf8 [] = String/Nil
String/decode_utf8 bytes =
  let (got, rest) = (Utf8/decode_character bytes)
  match rest {
    List/Nil: (String/Cons got String/Nil)
    List/Cons: (String/Cons got (String/decode_utf8 rest))
  }

#{ Decodes a utf-8 character, returns a tuple containing the rune and the rest of the byte sequence. #}
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
          (r, rest)
        } else {
          (Utf8/REPLACEMENT_CHARACTER, rest)
        }
      }
    }
  }

#{ Encodes a string to a sequence of bytes using utf-8 encoding. #}
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

#{ Decodes a sequence of bytes to a String using ascii encoding. #}
String/decode_ascii (bytes: (List u24)) : String
String/decode_ascii (List/Cons x xs) = (String/Cons x (String/decode_ascii xs))
String/decode_ascii (List/Nil)       = (String/Nil)

#{ Encodes a string to a sequence of bytes using ascii encoding. #}
String/encode_ascii (str: String) : (List u24)
String/encode_ascii (String/Cons x xs) = (List/Cons x (String/encode_ascii xs))
String/encode_ascii (String/Nil)       = (List/Nil)

#{ Math #}

#{
  Math/PI() -> f24
  The Pi (π) constant.
#}
def Math/PI() -> f24:
  return 3.1415926535

def Math/E() -> f24:
  return 2.718281828

#{
  Math/log(x: f24, base: f24) -> f24
  Computes the logarithm of `x` with the specified `base`.
#}
hvm Math/log -> (f24 -> f24 -> f24):
  (x ($([|] $(x ret)) ret))

#{
  Math/atan2(x: f24, y: f24) -> f24
  Computes the arctangent of `y / x`.
  Has the same behaviour as `atan2f` in the C math lib.
#}
hvm Math/atan2 -> (f24 -> f24 -> f24):
  ($([&] $(y ret)) (y ret))


#{
  Math/sin(a: f24) -> f24
  Computes the sine of the given angle in radians.
#}
hvm Math/sin -> (f24 -> f24):
  ($([<<0x0] a) a)

#{
  Math/cos(a: f24) -> f24
  Computes the cosine of the given angle in radians.
#}
hvm Math/cos -> (f24 -> f24):
  (a b)
  & @Math/PI ~ $([:/2.0] $([-] $(a $([<<0x0] b))))

#{
  Math/tan(a: f24) -> f24
  Computes the tangent of the given angle in radians.
#}
hvm Math/tan -> (f24 -> f24):
  ($([>>0x0] a) a)

#{ Computes the cotangent of the given angle in radians. #}
Math/cot (a: f24) : f24 =
  (/ 1.0 (Math/tan a))

#{ Computes the secant of the given angle in radians. #}
Math/sec (a: f24) : f24 =
  (/ 1.0 (Math/cos a))

#{ Computes the cosecant of the given angle in radians. #}
Math/csc (a: f24) : f24 =
  (/ 1.0 (Math/sin a))

#{ Computes the arctangent of the given angle. #}
Math/atan (a: f24) : f24 =
  (Math/atan2 a 1.0)

#{ Computes the arcsine of the given angle. #}
Math/asin (a: f24) : f24 =
  (Math/atan2 a (Math/sqrt (- 1.0 (* a a))))

#{ Computes the arccosine of the given angle. #}
Math/acos (a: f24) : f24 =
  (Math/atan2 (Math/sqrt (- 1.0 (* a a))) a)

#{ Converts degrees to radians. #}
Math/radians (a: f24) : f24 =
  (* a (/ Math/PI 180.0))

#{ Computes the square root  of the given number. #}
Math/sqrt (n: f24) : f24 =
  (** n 0.5)

#{ Round float up to the nearest integer. #}
def Math/ceil(n: f24) -> f24:
  i_n = i24/to_f24(f24/to_i24(n))
  if n <= i_n:
    return i_n
  else:
    return i_n + 1.0
 
#{ Round float down to the nearest integer. #}
def Math/floor(n: f24) -> f24:
  i_n = i24/to_f24(f24/to_i24(n))
  if n < i_n:
    return i_n - 1.0
  else:
    return i_n

#{ Round float to the nearest integer. #}
def Math/round(n: f24) -> f24:
  i_n = i24/to_f24(f24/to_i24(n))
  if (n - i_n) < 0.5:
    return Math/floor(n)
  else:
    return Math/ceil(n)
