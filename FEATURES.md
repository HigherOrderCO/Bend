## Features

Bend offers two flavors of syntax, the user-friendly python-like syntax "Imp" (the default) and the core ML/Haskell-like syntax "Fun".
You can read the full reference for both of them [here](docs/syntax.md), but these examples will use the first one.

To see some more complex examples programs, check out the [examples](examples/) folder.

### Basic features

We can start with a basic program that adds the numbers 3 and 2.

```py
def main() -> u24:
  return 2 + 3
```

Running this program will show the number 5.
Be careful with `run` since it will not show any warnings by default. Before running a new program, it's useful to first `check` it.

Bend programs consist of a series of function definitions, always starting with a function called `main` or `Main`.

Functions can receive arguments both directly and using a lambda abstraction.

```py
# These two are equivalent
def add(x: u24, y: u24) -> u24:
  return x + y

def add2() -> (u24 -> u24 -> u24): 
  return lambda x, y: x + y
```

You can then call this function like this:

```py
def main() -> u24:
  sum = add(2, 3)
  return sum
```

### Data types

You can bundle multiple values into a single value using a tuple or a struct.

```py
# With a tuple
def tuple_fst(x: Any) -> Any:
  # This destructures the tuple into the two values it holds.
  # '*' means that the value is discarded and not bound to any variable.
  (fst, *) = x
  return fst

# With an object (similar to what other languages call a struct, a class or a record)
object Pair { fst, snd }

def Pair/fst(x: Pair) -> Any:
  match x:
    case Pair:
      return x.fst

# We can also access the fields of an object after we `open` it.
def Pair/fst_2(x: Paul) -> Any:
  open Pair: x
  return x.fst

# This is how we can create new objects.
def Pair/with_one(x: Pair) -> Pair:
  return Pair{ fst: x, snd: 1 }

# The function can be named anything, but by convention we use Type/function_name.
def Pair/swap(x: Pair) -> Pair:
  open Pair: x
  # We can also call the constructor like any normal function.
  return Pair(x.snd, x.fst)
```

For more complicated data structures, we can use `type` to define algebraic data types.

```py
type MyTree:
  Node { val, ~left, ~right }
  Leaf
```

This defines a constructor function for each variant of the type, with names `MyTree/Node` and `MyTree/Leaf`.

Like most things in bend (except tuples and numbers), types defined with `type` and `object` become lambda encoded functions.
You can read how this is done internally by the compiler in [Defining data types](docs/defining-data-types.md) and [Pattern matching](docs/pattern-matching.md).

### Pattern matching

We can pattern match on values of a data type to perform different actions depending on the variant of the value.

```py
def Maybe/or_default(x: Maybe(T), default: T) -> T:
  match x:
    case Maybe/Some:
      # We can access the fields of the variant using 'matched.field'
      return x.value
    case Maybe/None:
      return default
```

### Folding and bending

We use `~` to indicate that a field is recursive.
This allows us to easily create and consume these recursive data structures with `bend` and `fold`.

`fold` is a recursive `match` that you can use to transform and consume data structures.
`bend` is a pure recursive loop that is very useful for generating data structures.

```py
def MyTree.sum(x: MyTree) -> u24:
  # Sum all the values in the tree.
  fold x:
    # The fold is implicitly called for fields marked with '~' in their definition.
    case MyTree/Node:
      return x.val + x.left + x.right
    case MyTree/Leaf:
      return 0

def main() -> u24:
  bend val = 0:
    when val < 10:
      # 'fork' calls the bend recursively with the provided values.
      x = MyTree/Node { val:val, left:fork(val + 1), right:fork(val + 1) }
    else:
      # 'else' is the base case, when the condition fails.
      x = MyTree/Leaf

  return MyTree.sum(x)
```

These are equivalent to inline recursive functions that create a tree and consume it.

```py
def MyTree.sum(x: MyTree) -> u24:
  match x:
    case MyTree/Node:
      return x.val + MyTree.sum(x.left) + MyTree.sum(x.right)
    case MyTree/Leaf:
      return 0

def main_bend(val: u24) -> MyTree:
  if val < 10:
    return MyTree/Node(val, main_bend(val + 1), main_bend(val + 1))
  else:
    return MyTree/Leaf

def main() -> u24:
  x = main_bend(0)
  return MyTree.sum(x)
```

Making your program around folding trees is a very good way of making it parallelizable, since each core can be dispatched to work on a different branch of the tree.

You can also pass some state variables to `fold` just like the variables used in a `bend`.
If you give a `fold` some state, then you necessarily need to pass it by calling the folded fields of the matched value, like passing an additional argument to the fold call.

```py
# This function substitutes each value in the tree with the sum of all the values before it.
def MyTree.map_sum(x: MyTree) -> MyTree:
  acc = 0
  fold x with acc:
    case MyTree/Node:
      # `x.left` and `x.right` are called with the new state value.
      # Note that values are copied if you use them more than once, so you don't want to pass something very large.
      return MyTree/Node{ val: x.val + acc, left: x.left(x.val + acc), right: x.right(x.val + acc) }
    case MyTree/Leaf:
      return x
```

This allows `fold` to be a very powerful and generic tool that can be used to implement most pure data transformations.

### Some caveats and limitations

_Attention_: Note that despite the ADT syntax sugars, Bend is an _untyped_ language and the compiler will not stop you from using values incorrectly, which can lead to very unexpected results.
For example, the following program will compile just fine even though `!=` is only defined for native numbers:

```py
def main():
  bend val = [0, 1, 2, 3]:
    when val != []:
      match val:
        case List/Cons:
          x = val.head + fork(val.tail)
        case List/Nil:
          x = 0
    else:
      x = 0
  return x
```

Running this program will show `λ* *` and not the expected `6`.

It's also important to note that Bend is linear (technically affine), meaning that every variable is only used once. When a variable is used more than once, the compiler will automatically insert a duplication.
Duplications efficiently share the same value between two locations, only cloning a value when it's actually needed, but their exact behaviour is slightly more complicated than that and escapes normal lambda-calculus rules.
You can read more about it in [Dups and sups](docs/dups-and-sups.md) and learn how pattern matching avoids this problem in [Pattern matching](docs/pattern-matching.md).

To use a variable twice without duplicating it, you can use a `use` statement.
It inlines clones of some value in the statements that follow it.

```py
def foo(x: Any) -> (Any, Any):
  use result = (1, x)
  return (result, result)

# Is equivalent to
def foo(x: Any) -> (Any, Any):
  return ((1, x), (1, x))
```

Note that any variable in the `use` will end up being duplicated.

Bend supports recursive functions of unrestricted depth:

```py
def native_num_to_adt(n: u24) -> Nat:
  if n == 0:
    return Nat/Zero
  else:
    return Nat/Succ(native_num_to_adt(n - 1))
```

If your recursive function is not based on pattern matching syntax (like `if`, `match`, `fold`, etc) you have to be careful to avoid an infinite loop.

```py
# A scott-encoded list folding function
# Writing it like this will cause an infinite loop.
def scott_list.add(xs: List, add: u24) -> List:
  return xs( λxs.head xs.tail: λc n: (c (xs.head + add), scott_list.add(xs.tail, add)))

# Instead we want to write it like this;
def scott_list.add(xs: List, add: u24) -> List:
  return xs(
    λxs.head xs.tail: λadd: λc n: (c (xs.head + add) scott_list.sum(xs.tail, add)),
    λadd: λc λn: n,
    add
  )
```

Since Bend is eagerly executed, some situations will cause function applications to always be expanded, which can lead to looping situations.
You can read how to avoid this in [Lazy definitions](docs/lazy-definitions.md).

### Numbers

Bend has native numbers and operations.

```py
def main() -> (u24, i24, f24):
  a = 1      # A 24 bit unsigned integer.
  b = +2     # A 24 bit signed integer.
  c = -3     # Another signed integer, but with negative value.
  d = 1.0    # A 24 bit floating point number.
  e = +0.001 # Also a float.
  return (a * 2, b - c, d / e)
```

Unsigned numbers are written as just the number.
Signed numbers are written with a `+` or `-` sign.
Floating point numbers must have the decimal point `.` and can optionally take a sign `+` or `-`.

The three number types are fundamentally different.
If you mix two numbers of different types HVM will interpret the binary representation of one of them incorrectly, leading to incorrect results. Which number is interpreted incorrectly depends on the situation and shouldn't be relied on for now.

Bend now has a way to convert between the different number types!
Here's some of the builtin functions you can use to cast any native number into the corresponding type:

```py
def main() -> _:
  x = f24/to_i24(1.0)
  y = u24/to_f24(2)
  z = i24/to_u24(-3)

  return (x, y, z)
```
You can find the other casting functions and their declarations at [builtins.md](docs/builtins.md).
### Other builtin types

Bend has Lists and Strings, which support Unicode characters.

# These are the definitions of the builtin types.
```py
type String:
  Cons { head, ~tail }
  Nil
type List:
  Cons { head, ~tail }
  Nil
```

```py
# Here's an example of a List of Strings
def main() -> List(String):
  return ["You: Hello, 🌎", "🌎: Hello, user"]
```

A string is desugared to a String data type containing two constructors, `String/Cons` and `String/Nil`.
List also becomes a type with two constructors, `List/Cons` and `List/Nil`.

```py
# When you write this
def StrEx() -> String:
  return "Hello"
def ids() -> List(u24):
  return [1, 2, 3]

# The compiler converts it to this
def StrEx() -> String:
  return String/Cons('H', String/Cons('e', String/Cons('l', String/Cons('l', String/Cons('o', String/Nil)))))
def ids() -> List(u24):
  return List/Cons(1, List/Cons(2, List/Cons(3, List/Nil)))
```

Characters are delimited by `'` `'` and support Unicode escape sequences. They are encoded as a U24 with the unicode codepoint as their value.

```py
# These two are equivalent
def chars() -> List(u24):
  return ['A', '\u{4242}', '🌎']

def chars2()  -> List(u24):
  return [65, 0x4242, 0x1F30E]
```

Bend has a built-in binary tree Map data structure where the key is a `u24` value, meaning you can use numbers, characters, and symbols as keys.

Maps are delimited by `{` `}` and its entries are separated by commas. A key-value entry in a map is denoted using a colon `:`. For example:

```py
{ 42: [4, 2] } # 42 is the key and [4, 2] is the value
```

A Map is desugared to a Map data type containing two constructors `Map/Leaf` and `Map/Node`.

```py
# When you write this
def empty_map() -> Map(T):
  return {}

def init_map() -> Map(String):
  return { 1: "one", 2: "two"}

def main() -> String:
  map = init_map
  one = map[1]    # map getter syntax
  map[0] = "zero" # map setter syntax
  return one

# The compiler converts it to this
def empty_map() -> Map(T):
  return Map/Leaf

def init_map() -> Map(String): 
  map = Map/set(Map/Leaf, 1, "one")
  map = Map/set(map, 2, "two")
  return map

def main() -> String:
  map = init_map
  (one, map) = Map/get(map, 1)
  map = Map/set(map, 0, "zero")
  return one

# The builtin Map type definition
type Map(T):
  Node { value: Maybe(T), ~left: Map(T), ~right: Map(T) }
  Leaf 
```

Notice that the getter and setter syntax induces an order on things using the map, since every get or set operation depends on the value of the previous map.
> **_NOTE:_** Do not get mistaken with lists creation syntax, that also uses `[` `]`.

### Mixing syntaxes

As was said in the beginning, Bend offers two flavors of syntax.
You can mix and match them freely in your program, as long as each function uses only one flavor.

```py
type Bool:
  True
  False

def is_odd(x: u24) -> Bool:
  switch x:
    case 0:
      return Bool/False
    case _:
      return is_even(x-1)

(is_even n) = switch n {
  0: Bool/True
  _: (is_odd n-1)
}

main = (is_odd 20)
```

### More features

Key:

- &#128215;: Basic resources
- &#128217;: Intermediate resources
- &#128213;: Advanced resources

Other features are described in the following documentation files:

- &#128215; Lazy definitions: [Making recursive definitions lazy](docs/lazy-definitions.md)
- &#128215; Data types: [Defining data types](docs/defining-data-types.md)
- &#128215; Pattern matching: [Pattern matching](docs/pattern-matching.md)
- &#128215; Native numbers and operations: [Native numbers](docs/native-numbers.md)
- &#128215; Builtin definitions: [Builtins](docs/builtins.md)
- &#128215; CLI arguments: [CLI arguments](docs/cli-arguments.md)
- &#128217; Duplications and superpositions: [Dups and sups](docs/dups-and-sups.md)
- &#128217; Scopeless lambdas: [Using scopeless lambdas](docs/using-scopeless-lambdas.md)
- &#128213; Fusing functions: [Writing fusing functions](docs/writing-fusing-functions.md)

## Further reading

- &#128217; [Compilation and readback](docs/compilation-and-readback.md)
- &#128217; [Old HVM wiki learning material](https://github.com/HigherOrderCO/HVM/wiki/HVM-Wiki). It is outdated, but it can still teach you some of the basics.
