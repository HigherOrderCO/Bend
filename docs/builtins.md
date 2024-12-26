> this is a WIP based on [Builtins.bend](https://github.com/HigherOrderCO/Bend/blob/main/src/fun/builtins.bend).

# Built-in Types and Functions

**Bend** built-in types and functions, this document serves as a reference guide. Read more at [FEATURES.md](https://github.com/HigherOrderCO/Bend/blob/main/FEATURES.md).

## String

```python
type String = (Cons head ~tail) | (Nil)
```

- **Nil**: Represents an empty string.
- **Cons head ~tail**: Represents a string with a `head` character and a `tail` string.

### Syntax

A String literal is surrounded with `"`. Accepts the same values as characters literals.

```
"Hello, World!"
```

### Functions

#### String/equals

Checks if two strings are equal.

```python
def String/equals(s1: String, s2: String) -> u24
```

#### String/split

Splits a string into a list of strings based on the given delimiter.

```python
def String/split(s: String, delimiter: u24) -> [String]
```

## List

```python
type List = (Cons head ~tail) | (Nil)
```

- **Nil**: Represents an empty list.
- **Cons head ~tail**: Represents a list with a `head` element and a `tail` list.

### Syntax

A List of values can be written using `[ ]`, it can have multiple values inside, using `,` you can divide its value in a list of multiple elements.

```
["This", "List", "Has", "Multiple", "Values"]
```

### Functions

#### List/length

```python
def List/length(list: [a]) -> (length: u24, list: [a])
```

Returns a tuple containing the length and the list itself.

#### List/reverse

```python
def List/reverse(list: [a]) -> [a]
```

Reverses the elements of a list.

#### List/flatten

```python
def List/flatten(list: [[a]]) -> [a]
```

Returns a flattened list from a list of lists. Example:

```python
List/flatten([[1], [2, 3], [4]])

# Result: [1, 2, 3, 4]
```

#### List/concat

```python
def List/concat(xs: [a], ys: [a]) -> [a]
```

Appends two lists together. Example:

```python
List/concat([1, 2], [4, 5])

# Result: [1, 2, 4, 5]
```

#### List/filter

Filters a list based on a predicate function.

```python
List/filter(xs: List(T), pred: T -> Bool) -> List(T)
```

#### List/split_once

Splits a list into two lists at the first occurrence of a value.

```python
List/split_once(xs: List(T), val: T) -> (Result(List(T), List(T)))
```

## Result

```python
type Result<A, B>:
  Ok { val: A }
  Err { val: B }
```

### Result/unwrap

Returns the inner value of `Result/Ok` or `Result/Err`.

If the types `A` and `B` are different, should only be used in type unsafe programs or when only one variant is guaranteed to happen.

```python
def Result/unwrap(result: Result<A, B>): A || B
```

## Tree

```python
type Tree:
  Node { ~left, ~right }
  Leaf { value }
```

**`Tree`** represents a tree with values stored in the leaves.
Trees are a structure that naturally lends itself to parallel recursion, so writing your problem in terms of trees is a good first approach to parallelize your code.

- **Node { ~left ~right }**: Represents a tree node with `left` and `right` subtrees.
- **Leaf { value }**: Represents one of the ends of the tree, storing `value`.

#### Syntax

**Bend** provides the `![]` operator to create tree branches and the `!` operator to create a tree leaf.

```py
# ![a, b] => Equivalent to Tree/Node { left: a, right: b }
# !x      => Equivalent to Tree/Leaf { value: x }
tree = ![![!1, !2],![!3, !4]]
```

Technically your trees don't need to end with leaves, but if you don't, your program will be very hard to reason about.
## Maybe

```python
type Maybe(T):
  Some{ value }
  None 
```
**`Maybe`** is a structure that may or not contain a value. It is meant to be used as a return type for functions that can fail. This way you don't need to resort to unreachable() in order to handle errors.

#### Syntax
Here's how you create a new `Maybe` containing the Nat value of 1:
```python
maybe = Maybe/Some(Nat/Succ(Nat/Zero))
```
## Maybe functions

### Maybe/unwrap
Maybe has a builtin function that returns the value inside the `Maybe` if it is `Some`, and returns `unreachable()` if it is `None`.
```python
def Maybe/unwrap(m: Maybe(T)) -> T:
  match m:
    case Maybe/Some:
      return m.val
    case Maybe/None:
      return unreachable()
```
## Map

```python
type Map(T):
  Node { value: Maybe(T), ~left: Map(T), ~right: Map(T) }
  Leaf  
```

**`Map`** represents a tree with values stored in the branches.
It is meant to be used as an efficient map data structure with integer keys and O(log n) read and write operations.

- **Node { value: Maybe(T), ~left: Map(T), ~right: Map(T) }**: Represents a map node with a `Maybe` and `left` and `right` subtrees. Empty nodes have `Maybe/None` stored in the `value` field, whilst non-empty nodes have `Maybe/Some` stored in the `value` field.
- **Leaf**: Represents an unwritten, empty portion of the map.

#### Syntax

Here's how you create a new `Map` with some initial values.:

```python
{ 0: 4, `hi`: "bye", 'c': 2 + 3 }
```

The keys must be `U24` numbers, and can be given as literals or any other expression that evaluates to a `U24`.

The values can be anything, but storing data of different types in a `Map` will make it harder for you to reason about it.

You can read and write a value of a map with the `[]` operator:

```python
map = { 0: "zero", 1: "one", 2: "two", 3: "three" }
map[0] = "not zero"
map[1] = 2
map[2] = 3
map[3] = map[1] + map[map[1]]
```

Here, `map` must be the name of the `Map` variable, and the keys inside `[]` can be any expression that evaluates to a `U24`.

## Map functions

### Map/empty

Initializes an empty map.

```python
Map/empty = Map/Leaf
```

### Map/get

Retrieves a `value` from the `map` based on the `key`.
Returns a tuple with the value and the `map` unchanged.

```rust
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
```

#### Syntax

Considering the following map

```python
{ 0: "hello", 1: "bye", 2: "maybe", 3: "yes"}
```

The `get` function can be written as

```
return x[0]  # Gets the value of the key 0
```

And the value resultant from the get function would be:

```
"hello"
```

### Map/set

```rust
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
```

#### Syntax

Considering the following tree

```python
{ 0: "hello", 1: "bye", 2: "maybe", 3: "yes"}
```

The `set` function can be written as

```py
x[0] = "swapped"     # Assigns the key 0 to the value "swapped"
```

And the value resultant from the get function would be:

```py
{ 0: "swapped", 1: "bye", 2: "maybe", 3: "yes"}
```

If there's no matching `key` in the tree, it would add a new branch to that tree with the value `set`

```py
x[4] = "added"     # Assigns the key 4 to the value "added"
```

The new tree

```py
{ 0: "swapped", 1: "bye", 2: "maybe", 3: "yes", 4: "added"}
```

### Map/map

Applies a function to a value in the map.
Returns the map with the value mapped.

```rust
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
```

#### Syntax

With the same map that we `set` in the previous section, we can map it's values with `@=`:

```py
x[0] @= lambda y: String/concat(y, " and mapped")
# x[0] now contains "swapped and mapped"
```


### Map/contains
Checks if a `map` contains a given `key` and returns 0 or 1 as a `u24` number and the `map` unchanged.
```python
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
```

#### Syntax

With the same map that we `set` in the previous section, we can call the function `Map/contains` explicitly:

```python
(num, map) = Map/contains(m, key)
return num
```
Whilst the `num` variable will contain 0 or 1 depending on if the key is in the map or not.


## Nat

```python
type Nat = (Succ ~pred) | (Zero)
```

- **Succ ~pred**: Represents a natural number successor.
- **Zero**: Represents the natural number zero.

### Syntax

A Natural Number can be written with literals with a `#` before the literal number.

```
#1337
```

## DiffList

DiffList is a list that has constant time prepends (cons), appends and concatenation, but can't be pattern matched.

It is implemented as a function that receives a list to be appended to the last element of the DiffList.

For example, the list `List/Cons(1, List/Cons(2, List/Nil))` can be written as the difference list `lambda x: List/Cons(1, List/Cons(2, x))`.

### Functions

#### DiffList/new

Creates a new difference list.

```python
def DiffList/new() -> (List(T) -> List(T))
```

#### DiffList/append

Appends a value to the end of the difference list.

```python
def DiffList/append(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T))
```

#### DiffList/cons

Appends a value to the beginning of the difference list.

```python
def DiffList/cons(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T))
```

#### DiffList/to_list

Converts a difference list to a regular cons list.

```python
def DiffList/to_list(diff: List(T) -> List(T)) -> (List(T))
```

## IO

The basic builtin IO functions are under development and will be stable in the next milestone.

Here is the current list of functions, but be aware that they may change in the near future.

### Printing

```python
def IO/print(text)
```

Prints the string `text` to the standard output, encoded with utf-8.

### Input

```python
def IO/input() -> String
```

Reads characters from the standard input until a newline is found.

Returns the read input as a String decoded with utf-8.

### File IO

#### File open

```python
def IO/FS/open(path, mode)
```

Opens a file with with `path` being given as a string and `mode` being a string with the mode to open the file in. The mode should be one of the following:

- `"r"`: Read mode
- `"w"`: Write mode (write at the beginning of the file, overwriting any existing content)
- `"a"`: Append mode (write at the end of the file)
- `"r+"`: Read and write mode
- `"w+"`: Read and write mode
- `"a+"`: Read and append mode

Returns an U24 with the file descriptor. File descriptors are not necessarily the same as the ones assigned by the operating system, but rather unique identifiers internal to Bend's runtime.

#### File descriptors for standard files

The standard input/output files are always open and assigned the following file descriptors:

- `IO/FS/STDIN = 0`: Standard input
- `IO/FS/STDOUT = 1`: Standard output
- `IO/FS/STDERR = 2`: Standard error

#### File close

```python
def IO/FS/close(file)
```

Closes the file with the given `file` descriptor.

#### File read

```python
def IO/FS/read(file, num_bytes)
```

Reads `num_bytes` bytes from the file with the given `file` descriptor.

Returns a list of U24 with each element representing a byte read from the file.

```python
def IO/FS/read_line(file)
```

Reads a line from the file with the given `file` descriptor.

Returns a list of U24 with each element representing a byte read from the file.

```python
def IO/FS/read_until_end(file)
```

Reads until the end of the file with the given `file` descriptor.

Returns a list of U24 with each element representing a byte read from the file.

```python
def IO/FS/read_file(path)
```

Reads an entire file with the given `path` and returns a list of U24 with each element representing a byte read from the file.

#### File write

```python
def IO/FS/write(file, bytes)
```

Writes `bytes`, a list of U24 with each element representing a byte, to the file with the given `file` descriptor.

Returns nothing (`*`).

```python
def IO/FS/write_file(path, bytes)
```

Writes `bytes`, a list of U24 with each element representing a byte, as the entire content of the file with the given `path`.

#### File seek

```python
def IO/FS/seek(file, offset, mode)
```

Moves the current position of the file with the given `file` descriptor to the given `offset`, an I24 or U24 number, in bytes.

`mode` can be one of the following:

- `IO/FS/SEEK_SET = 0`: Seek from start of file
- `IO/FS/SEEK_CUR = 1`: Seek from current position
- `IO/FS/SEEK_END = 2`: Seek from end of file

Returns nothing (`*`).

#### File flush

```python
def IO/FS/flush(file)
```

Flushes the file with the given `file` descriptor.

Returns nothing (`*`).

### Dinamically linked libraries

It's possible to dynamically load shared objects (libraries) with functions that implement the Bend IO interface.
You can read more on how to implement these libraries in the [Dynamically linked libraries and foreign functions](docs/ffi.md) documentation.

#### IO/DyLib/open

```py
def IO/DyLib/open(path: String, lazy: u24) -> u24
```

Loads a dynamic library file.

- `path` is the path to the library file.
- `lazy` is a boolean encoded as a `u24` that determines if all functions are loaded lazily (`1`) or upfront (`0`).
- Returns an unique id to the library object encoded as a `u24`.

#### IO/DyLib/call

```py
def IO/DyLib/call(dl: u24, fn: String, args: Any) -> Any
```

Calls a function of a previously opened library.

- `dl` is the id of the library object.
- `fn` is the name of the function in the library.
- `args` are the arguments to the function. The expected values depend on the called function.
- The returned value is determined by the called function.

#### IO/DyLib/close

```py
def IO/DyLib/close(dl: u24) -> None
```

Closes a previously open library.

- `dl` is the id of the library object.
- Returns nothing (`*`).

## Native number casting

### to_f24

```py
def to_f24(x: any number) -> f24
```

Casts any native number to an f24.

### to_u24

```py
def to_u24(x: any number) -> u24
```

Casts any native number to a u24.

### to_i24

```py
def to_i24(x: any number) -> i24
```

Casts any native number to an i24.

## String encoding / decoding

### String/decode_utf8

```py
def String/decode_utf8(bytes: [u24]) -> String
```

Decodes a sequence of bytes to a String using utf-8 encoding.

### String/decode_ascii

```py
def String/decode_ascii(bytes: [u24]) -> String
```

Decodes a sequence of bytes to a String using ascii encoding.

### String/encode_utf8

```py
def String/encode_utf8(s: String) -> [u24]
```

Encodes a String to a sequence of bytes using utf-8 encoding.

### String/encode_ascii

```py
def String/encode_ascii(s: String) -> [u24]
```

Encodes a String to a sequence of bytes using ascii encoding.

### Utf8/decode_character

```py
def Utf8/decode_character(bytes: [u24]) -> (rune: u24, rest: [u24])
```

Decodes a utf-8 character, returns a tuple containing the rune and the rest of the byte sequence.

### Utf8/REPLACEMENT_CHARACTER

```py
def Utf8/REPLACEMENT_CHARACTER: u24 = '\u{FFFD}'
```

## Math

### Math/log

```py
def Math/log(x: f24, base: f24) -> f24
```

Computes the logarithm of `x` with the specified `base`.

### Math/atan2

```py
def Math/atan2(x: f24, y: f24) -> f24
```

Computes the arctangent of `y / x`.

Has the same behaviour as `atan2f` in the C math lib.

### Math/PI

Defines the Pi constant.

```py
def Math/PI: f24 = 3.1415926535
```

### Math/E

Euler's number

```py
def Math/E: f24 = 2.718281828
```

### Math/sin

Computes the sine of the given angle in radians.

```py
def Math/sin(a: f24) -> f24
```

### Math/cos

Computes the cosine of the given angle in radians.

```py
def Math/cos(a: f24) -> f24
```

### Math/tan

Computes the tangent of the given angle in radians.

```py
def Math/tan(a: f24) -> f24
```

### Math/cot

Computes the cotangent of the given angle in radians.

```py
def Math/cot(a: f24) -> f24
```

### Math/sec

Computes the secant of the given angle in radians.

```py
def Math/sec(a: f24) -> f24
```

### Math/csc

Computes the cosecant of the given angle in radians.

```py
def Math/csc(a: f24) -> f24
```

### Math/atan

Computes the arctangent of the given angle.

```py
def Math/atan(a: f24) -> f24
```

### Math/asin

Computes the arcsine of the given angle.

```py
def Math/asin(a: f24) -> f24
```

### Math/acos

Computes the arccosine of the given angle.

```py
def Math/acos(a: f24) -> f24
```

### Math/radians

Converts degrees to radians.

```py
def Math/radians(a: f24) -> f24
```

### Math/sqrt

Computes the square root of the given number.

```py
def Math/sqrt(n: f24) -> f24
```

### Math/ceil

Round float up to the nearest integer.

```py
def Math/ceil(n: f24) -> f24
```

### Math/floor

Round float down to the nearest integer.

```py
def Math/floor(n: f24) -> f24
```

### Math/round

Round float to the nearest integer.

```py
def Math/round(n: f24) -> f24
```

## Lazy thunks

You can force a function call to be evaluated lazily by wrapping it in a lazy thunk.
In Bend, this can be expressed as `lambda x: x(my_function, arg1, arg2, ...)`.

To evaluate the thunk, you can use the `undefer` function or apply `lambda x: x` to it.
