> this is a WIP based on [Builtins.bend](https://github.com/HigherOrderCO/Bend/blob/main/src/fun/builtins.bend).

# Built-in Types and Functions

**Bend** built-in types and functions, this document serves as a reference guide. Read more at [FEATURES.md](https://github.com/HigherOrderCO/Bend/blob/main/FEATURES.md).

## String

```python
type String:
  Nil
  Cons { head: u24, ~tail: String }
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

```python
#{
  Checks if two strings are equal.
#}
def String/equals (s1: String) (s2: String) : u24
```

#### String/split

```python
#{
  Splits a string into a list of strings based on the given delimiter.
#}
String/split (s: String) (delimiter: u24) : (List String)
```

## List

```python
type List(T):
  Nil
  Cons { head: T, ~tail: List(T) }
```

- **Nil**: Represents an empty list.
- **Cons head ~tail**: Represents a list with a `head` element and a `tail` list.
- **T**: Represents the type of the elements in the list.

### Syntax

A List of values can be written using `[ ]`, it can have multiple values inside, using `,` you can divide its value in a list of multiple elements.

```
["This", "List", "Has", "Multiple", "Values"]
```

### Functions

#### List/length

```python
#{
  Returns a tuple containing the length and the list itself.
#}
def List/length(xs: List(T)) -> (u24, List(T)):
```



#### List/reverse

```python
#{
  Reverses the elements of a list.
#}
def List/reverse(xs: List(T)) -> List(T):
```

#### List/flatten

```python
#{
  Returns a flattened list from a list of lists.
#}
List/flatten (xs: (List (List T))) : (List T)
```
Example:
```python
List/flatten([[1], [2, 3], [4]])

# Result: [1, 2, 3, 4]
```

#### List/concat

```python
#{
  Appends two lists together. 
#}
def List/concat(xs: (List T)) (ys: (List T)) : (List T)
```
Example:
```python
List/concat([1, 2], [4, 5])

# Result: [1, 2, 4, 5]
```

#### List/filter

```python
#{
  Filters a list based on a predicate function.
#}
List/filter(xs: List(T), pred: T -> Bool) -> List(T)
```

#### List/split_once

```python
#{
Splits a list into two lists at the first occurrence of a value.
#}
def List/split_once(xs: List(T), cond: T -> u24) -> (Result((List(T), List(T)), List(T))): 
```
Example:
```python
  # Split list at first even number
  list = [1,3,4,5,6]
  result = List/split_once(list, λx: x % 2 == 0)
  return result
  # Result: Result/Ok/tag ([1, 3], [5, 6])
```

## Result

```python
type (Result o e) = (Ok (val: o)) | (Err (val: e))
```

### Result/unwrap

Returns the inner value of `Result/Ok` or `Result/Err`.

If the types `A` and `B` are different, should only be used in type unsafe programs or when only one variant is guaranteed to happen.

```python
#{
Returns the inner value of `Result/Ok` or `Result/Err`.

If the types `A` and `B` are different, should only be used in type unsafe programs or when only one variant is guaranteed to happen.
#}
def Result/unwrap(res: Result(T, E)) -> Any:
```

## Tree

```python
type Tree(T):
  Node { ~left: Tree(T), ~right: Tree(T) }
  Leaf { value: T }
```

**`Tree`** represents a tree with values stored in the leaves.
Trees are a structure that naturally lends itself to parallel recursion, so writing your problem in terms of trees is a good first approach to parallelize your code.

- **Node { ~left ~right }**: Represents a tree node with `left` and `right` subtrees.
- **Leaf { value }**: Represents one of the ends of the tree, storing `value`.
- **T**: Represents the type of the elements in the tree.

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
```python
#{
Returns the value inside the `Maybe` if it is `Some`, and returns `unreachable()` if it is `None`.
#}  
def Maybe/unwrap(m: Maybe(T)) -> T
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
def main():
  return { 0: 4, `hi`: "bye", 'c': 2 + 3 }
```

The keys must be `U24` numbers, and can be given as literals or any other expression that evaluates to a `U24`.

As long as your function isn't typed, like the one in the example, the values can be anything. But storing data of different types in a `Map` will make it harder for you to reason about it.

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

```python
#{
  Initializes an empty map.
#} 
Map/empty = Map/Leaf
```

### Map/get



```rust
#{
  Retrieves a `value` from the `map` based on the `key` and returns a tuple with the value and the `map` unchanged. The logic for checking whether a value is or not contained in a `map` is not done in the `get` function, so if we try to get a key that is not in the map, the program will return `unreachable`. 
def Map/get (map: Map(T), key: u24) -> (T, Map(T))
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
#{
  Sets a value on a Map, returning the map with the value mapped.
#}
def Map/set (map: Map(T), key: u24, value: T) -> Map(T)
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


```rust
#{
  Applies a function to a value in the map and returns the map with the value mapped.
#}
def Map/map (map: Map(T), key: u24, f: T -> T) -> Map(T)
```

#### Syntax

With the same map that we `set` in the previous section, we can map it's values with `@=`:

```py
x[0] @= lambda y: String/concat(y, " and mapped")
# x[0] now contains "swapped and mapped"
```


### Map/contains

```python
#{
  Checks if a `map` contains a given `key` and returns 0 or 1 along with and  `map` unchanged.
#}
def Map/contains (map: Map(T), key: u24) -> (u24, Map(T))

#### Syntax

With the same map that we `set` in the previous section, we can call the function `Map/contains` explicitly:

```python
(num, map) = Map/contains(m, key)
return num
```
Whilst the `num` variable will contain 0 or 1 depending on if the key is in the map or not.


## Nat

```python
type Nat = (Succ ~(pred: Nat)) | (Zero)
```

- **Succ ~pred**: Represents a natural number successor.
- **Zero**: Represents the natural number zero.

## DiffList

DiffList is a list that has constant time prepends (cons), appends and concatenation, but can't be pattern matched.

It is implemented as a function that receives a list to be appended to the last element of the DiffList.

For example, the list `List/Cons(1, List/Cons(2, List/Nil))` can be written as the difference list `lambda x: List/Cons(1, List/Cons(2, x))`.

### Functions

#### DiffList/new


```python
#{
Creates a new difference list.
#}
def DiffList/new() -> (List(T) -> List(T))
```

#### DiffList/append


```python
#{
  Appends a value to the end of the difference list.
#}
def DiffList/append(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T))
```

#### DiffList/cons

```python
#{
  Appends a value to the beginning of the difference list.
#}
def DiffList/cons(diff: List(T) -> List(T), val: T) -> (List(T) -> List(T))
```

#### DiffList/to_list


```python
#{
  Converts a difference list to a regular cons list.
#}
def DiffList/to_list(diff: List(T) -> List(T)) -> (List(T))
```

## IO

The basic builtin IO functions are under development and will be stable in the next milestone.

Here is the current list of functions, but be aware that they may change in the near future.

### Printing

```python
#{
  Prints the string `text` to the standard output, encoded with utf-8.
#}
def IO/print(text: String) -> IO(None)
```


### Input

```python
#{
  Reads characters from the standard input until a newline is found.
  Returns the read input as a String decoded with utf-8.
#}
def IO/input() -> IO(Result(String, u24))
```



### File IO

#### File open

```python
#{
  Opens a file with with `path` being given as a string and `mode` being a string with the mode to open the file in. The mode should be one of the following:
#}
def IO/FS/open(path: String, mode: String) -> IO(Result(u24, u24))
```


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
#{
  Closes the file with the given `file` descriptor.
#}
def IO/FS/close(file: u24) -> IO(Result(None, u24))
```


#### File read

```python
#{
Reads `num_bytes` bytes from the file with the given `file` descriptor.
Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read(file: u24, num_bytes: u24) -> IO(Result(List(u24), u24))
```



```python
#{
  Reads a line from the file with the given `file` descriptor.
  Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read_line(fd: u24) -> IO(Result(List(u24), u24))
```



```python
#{
  Reads until the end of the file with the given `file` descriptor.
  Returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read_to_end(fd: u24) -> IO(Result(List(u24), u24))
```



```python
#{
  Reads an entire file with the given `path` and returns a list of U24 with each element representing a byte read from the file.
#}
def IO/FS/read_file(path: String) -> IO(Result(List(u24), u24))
```


#### File write

```python
#{
  Writes `bytes`, a list of U24 with each element representing a byte, to the file with the given `file` descriptor.
  Returns nothing (`*`).
#}
def IO/FS/write(file: u24, bytes: List(u24)) -> IO(Result(None, u24))
```

```python
#{
  Writes `bytes`, a list of U24 with each element representing a byte, as the entire content of the file with the given `path`.
#}
def IO/FS/write_file(path: String, bytes: List(u24)) -> IO(Result(None, u24))
```

#### File seek

```python
#{
  Moves the current position of the file with the given `file` descriptor to the given `offset`, an I24 or U24 number, in bytes.
#}
def IO/FS/seek(file: u24, offset: i24, mode: i24) -> IO(Result(None, u24)) 
```

`mode` can be one of the following:

- `IO/FS/SEEK_SET = 0`: Seek from start of file
- `IO/FS/SEEK_CUR = 1`: Seek from current position
- `IO/FS/SEEK_END = 2`: Seek from end of file

Returns nothing (`*`).

#### File flush

```python
#{
  Flushes the file with the given `file` descriptor.
  Returns nothing (`*`).
#}
def IO/FS/flush(file: u24) -> IO(Result(None, u24))
```

### Dinamically linked libraries

It's possible to dynamically load shared objects (libraries) with functions that implement the Bend IO interface.
You can read more on how to implement these libraries in the [Dynamically linked libraries and foreign functions](docs/ffi.md) documentation.

#### IO/DyLib/open

```py
#{
  Loads a dynamic library file.
#}
def IO/DyLib/open(path: String, lazy: u24) -> IO(Result(u24, String))
```
- `path` is the path to the library file.
- `lazy` is a boolean encoded as a `u24` that determines if all functions are loaded lazily (`1`) or upfront (`0`).
- Returns an unique id to the library object encoded as a `u24`.

#### IO/DyLib/call

```py
#{
  Calls a function of a previously opened library.
  - `dl` is the id of the library object.
  - `fn` is the name of the function in the library.
  - `args` are the arguments to the function. The expected values depend on the called function.
  - The returned value is determined by the called function.
#}
def IO/DyLib/call(dl: u24, fn: String, args: Any) -> IO(Result(Any, String))
```


#### IO/DyLib/close

```py
#{
  Closes a previously open library.
  - `dl` is the id of the library object.
  - Returns nothing (`*`).
#}
def IO/DyLib/close(dl: u24) -> IO(Result(None, String))
```


## Native number casting

### to_f24

```py
#{
  Casts an u24 number to an f24.
#}
def u24/to_f24 -> (u24 -> f24)

#{
  Casts an i24 number to an f24.
#}
def i24/to_f24 -> (i24 -> f24)
```
### to_u24

```py
#{
  Casts a f24 number to an u24.
#}
def f24/to_u24 -> (f24 -> u24)

#{
  Casts an i24 number to an u24.
#}
def i24/to_u24 -> (i24 -> u24)
```
### to_i24

```py
#{
  Casts an u24 number to an i24.
#}
def u24/to_i24 -> (u24 -> i24):
#{
  Casts a f24 number to an i24.
#}
def f24/to_i24 -> (f24 -> i24):
```

### to_string

```py
#{
  Casts an u24 native number to a string.
#}
def u24/to_string(n: u24) -> String:
```

## String encoding / decoding

### String/decode_utf8

```py
#{
  Decodes a sequence of bytes to a String using utf-8 encoding.
#}
String/decode_utf8 (bytes: (List u24)) : String
```


### String/decode_ascii

```py
#{
  Decodes a sequence of bytes to a String using ascii encoding.
#}
String/decode_ascii (bytes: (List u24)) : String
```


### String/encode_utf8

```py
#{
  Encodes a String to a sequence of bytes using utf-8 encoding.
#}
String/encode_utf8 (str: String) : (List u24)
```


### String/encode_ascii

```py
#{
  Encodes a String to a sequence of bytes using ascii encoding.
#}
String/encode_ascii (str: String) : (List u24)
```


### Utf8/decode_character

```py
#{
  Decodes a utf-8 character, returns a tuple containing the rune and the rest of the byte sequence.
#}
Utf8/decode_character (bytes: (List u24)) : (u24, (List u24))
```


### Utf8/REPLACEMENT_CHARACTER

```py
Utf8/REPLACEMENT_CHARACTER : u24 = '\u{FFFD}'
```

## Math

### Math/log

```py
#{
  Computes the logarithm of `x` with the specified `base`.
#}
def Math/log -> (f24 -> f24 -> f24)
```


### Math/atan2

```py
#{
  Computes the arctangent of `y / x`.
  Has the same behaviour as `atan2f` in the C math lib.
#}
def Math/atan2 -> (f24 -> f24 -> f24)
```


### Math/PI


```py
#{
  Defines the Pi constant.
#}
def Math/PI() -> f24
```

### Math/E


```py
#{
Euler's number
#}
def Math/E() -> f24
```

### Math/sin


```py
#{
  Computes the sine of the given angle in radians.
#}
def Math/sin -> (f24 -> f24)
```

### Math/cos


```py
#{
  Computes the cosine of the given angle in radians.
#}
def Math/cos -> (f24 -> f24)
```

### Math/tan


```py
#{
  Computes the tangent of the given angle in radians.
#}
def Math/tan -> (f24 -> f24)
```

### Math/cot


```py
#{
  Computes the cotangent of the given angle in radians.
#}
Math/cot (a: f24) : f24 
```

### Math/sec


```py
#{
  Computes the secant of the given angle in radians.
#}
Math/sec (a: f24) : f24 
```

### Math/csc


```py
#{
  Computes the cosecant of the given angle in radians.
#}
Math/csc (a: f24) : f24 
```

### Math/atan



```py
#{
  Computes the arctangent of the given angle.
#}
Math/atan (a: f24) : f24 
```

### Math/asin


```py
#{
  Computes the arcsine of the given angle.
#}
Math/asin (a: f24) : f24 
```

### Math/acos


```py
#{
  Computes the arccosine of the given angle.
#}
Math/acos (a: f24) : f24

### Math/radians


```py
#{
  Converts degrees to radians.
#}
Math/radians (a: f24) : f24 
```

### Math/sqrt


```py
#{
  Computes the square root of the given number.
#}
Math/sqrt (n: f24) : f24 
```

### Math/ceil


```py
#{
  Round float up to the nearest integer.
#}
def Math/ceil(n: f24) -> f24
```

### Math/floor


```py
#{
  Round float down to the nearest integer.
#}
def Math/floor(n: f24) -> f24
```

### Math/round


```py
#{
  Round float to the nearest integer.
#}
def Math/round(n: f24) -> f24
```

## Lazy thunks

You can force a function call to be evaluated lazily by wrapping it in a lazy thunk.
In Bend, this can be expressed as `lambda x: x(my_function, arg1, arg2, ...)`.

To evaluate the thunk, you can use the `undefer` function or apply `lambda x: x` to it.
