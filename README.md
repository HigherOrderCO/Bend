# Bend

Bend is a programming language that can run massively parallel programs on the GPU or the CPU using the power of interaction nets and the [HVM](https://github.com/HigherOrderCO/hvm).
With Bend, you can write programs for the GPU as easily as you'd write a normal program in your favorite language.

It is based on the [Interaction-Calculus](https://github.com/VictorTaelin/Interaction-Calculus#interaction-calculus), a variation of the untyped lambda calculus that compiles efficiently to interaction nets.

Currently Bend only supports strict/eager evaluation. If you need lazy, optimal evaluation, we recommend using [HVM1](https://github.com/HigherOrderCO/HVM1) for now.

## Installation

With the nightly version of rust installed, clone the repository:

```bash
git clone https://github.com/HigherOrderCO/bend.git

cd bend
```

Install using cargo:

```bash
cargo install --path . --locked
```

If you want to run programs directly from Bend, you also need to have [HVM](https://github.com/HigherOrderCO/hvm2) installed.

## Usage

| Command | Usage                 | Description                                                       |
| ------- | --------------------- | ----------------------------------------------------------------- |
| Check   | `bend check <file>`   | Checks if a program is valid                                      |
| GenHvm  | `bend gen-hvm <file>` | Compiles a program to HVM and outputs it to stdout                |
| Run     | `bend run <file>`     | Compiles and then runs a program with the Rust HVM implementation |
| Run-C   | `bend run-c <file>`   | Compiles and then runs a program with the C HVM implementation    |
| Run-Cu  | `bend run-cu <file>`  | Compiles and then runs a program with the Cuda HVM implementation |
| Gen-C   | `bend gen-c <file>`   | Compiles the program to standalone C                              |
| Gen-Cu  | `bend gen-cu <file>`  | Compiles the program to standalone Cuda                           |
| Desugar | `bend desugar <file>` | Desugars a program to the core syntax and outputs it to stdout    |

If your program uses IO, it should return an IO value and you need to run it with --io. See [using io](docs/using-io.md), for more details.
If your program doesn't return an IO value, then you should not run it with --io

If you want to compile a file to a file, just redirect the output with `>`:

```bash
bend compile <file.bend> > <file.hvm>
```

There are many compiler options that can be passed through the CLI. You can see the list of options [here](docs/compiler-options.md).

## Examples

Bend offers two flavors of syntax, a user-friendly python-like syntax (the default) and the core ML/Haskell-like syntax that's used internally by the compiler.
You can read the full reference for both of them [here](docs/syntax.md), but these examples will use the first one.

To see some more complex examples programs, check out the [examples](examples/) folder.

We can start with a basic program that adds the numbers 3 and 2.

```py
def main:
  return 2 + 3
```

Normalizing this program will show the number 5.
Be careful with `run` and `norm`, since they will not show any warnings by default. Before running a new program it's useful to first `check` it.

Bend programs consist of a series of function definitions, always starting with a function called `main` or `Main`.

Functions can receive arguments both directly and using a lambda abstraction.

```py
// These two are equivalent
def add(x, y):
  return x + y

def add2:
  return lambda x, y: x + y
```

You can then call this function like this:

```py
def main:
  sum = add(2, 3)
  return sum
```

You can bundle multiple values into a single value using a tuple or a struct.

```py
// With a tuple
def Tuple.fst(x):
  // This destructures the tuple into the two values it holds.
  // '*' means that the value is discarded and not bound to any variable.
  (fst, *) = x
  return fst

// With a struct
struct Pair(fst, snd):
def Pair.fst(x):
  match x:
    Pair:
      return x.fst

// We can also directly access the fields of a struct.
// This requires that we tell the compiler the type of the variable where it is defined.
def Pair.fst_2(x: Pair):
  return x.fst
```

For more complicated data structures, we can use `enum` to define a algebraic data types.

```py
enum MyTree:
  Node(val, ~left, ~right)
  Leaf
```

We can then pattern match on the enum to perform different actions depending on the variant of the value.

```py
def Maybe.or_default(x, default):
  match x:
    Maybe/some:
      // We can access the fields of the variant using 'matched.field'
      return x.val
    Maybe/none:
      return default
```

We use `~` to indicate that a field is recursive.
This allows us to easily create and consume these recursive data structures with `bend` and `fold`:

```py
def MyTree.sum(x):
  // Sum all the values in the tree.
  fold x:
    // The fold is implicitly called for fields marked with '~' in their definition.
    Node:
      return val + x.left + x.right
    Leaf:
      return 0

def main:
  bend val = 0 while val < 0:
    // 'go' calls the bend recursively with the provided values.
    x = Node(val=val, left=go(val + 1), right=go(val + 1))
  then:
    // 'then' is the base case, when the condition fails.
    x = Leaf

  return MyTree.sum(x)
```

These are equivalent to inline recursive functions that create a tree and consume it.

```py
def MyTree.sum(x):
  match x:
    Node:
      return x.val + MyTree.sum(x.left) + MyTree.sum(x.right)
    Leaf:
      return 0

def main_bend(val):
  if val < 0:
    return Node(val, main_bend(val + 1), main_bend(val + 1))
  else:
    return Leaf

def main:
  return main_bend(0)
```

Making your program around trees is a very good way of making it parallelizable, since each core can be dispatched to work on a different branch of the tree.

_Attention_: Note that despite the ADT syntax sugars, Bend is an _untyped_ language and the compiler will not stop you from using values incorrectly, which can lead to very unexpected results.
For example, the following program will compile just fine even though `!=` is only defined for native numbers:

```py
def main:
  bend val = [0, 1, 2, 3] while val != []:
    match val:
      List.cons:
        x = val.head + go(val.tail)
      List.nil:
        x = 0
  then:
    x = 0
  return x
```

Normalizing this program will show `λ* *` and not the expected `6`.

It's also important to note that Bend is linear (technically affine), meaning that every variable is only used once. When a variable is used more than once, the compiler will automatically insert a duplication.
Duplications efficiently share the same value between two locations, only cloning a value when it's actually needed, but their exact behaviour is slightly more complicated than that and escapes normal lambda-calculus rules.
You can read more about it in [Dups and sups](docs/dups-and-sups.md) and learn how pattern matching avoids this problem in [Pattern matching](docs/pattern-matching.md).

To use a variable twice without duplicating it, you can use a `use` statement.
It inlines clones of some value in the statements that follow it.

```py
def foo(x):
  use result = bar(1, x)
  return (result, result)

// Is equivalent to
def foo(x):
  return (bar(1, x), bar(1, x))
```

Note that any variable in the `use` will end up being duplicated.

Bend supports recursive functions of unrestricted depth:

```py
def native_num_to_adt(n):
  if n == 0:
    return Nat.zero
  else:
    return Nat.succ(native_num_to_adt(n - 1))
```

If your recursive function is not based on pattern matching syntax (like `if`, `match`, `fold`, etc) you have to be careful to avoid an infinite loop.
Since Bend is eagerly executed, some situations will cause function applications to always be expanded, which can lead to looping situations.
You can read how to avoid this in [Lazy definitions](docs/lazy-definitions.md).

Bend has native numbers and operations.

```py
def main:
  a = 1      // A 24 bit unsigned integer.
  b = +2     // A 24 bit signed integer.
  c = -3     // Another signed integer, but with negative value.
  d = 1.0    // A 24 bit floating point number.
  e = +0.001 // Also a float.
  return (a * 2, b - c, d / e)
```

`switch` pattern matches on unsigned native numbers:

```py
switch x = 4:
  // From '0' to n, ending with the default case '_'.
  0:  "zero"
  1:  "one"
  2:  "two"
  // The default case binds the name <arg>-<n>
  // where 'arg' is the name of the argument and 'n' is the next number.
  // In this case, it's 'x-3', which will have value (4 - 3) = 1
  _:  String.concat("other: ", (String.from_num x-3))
```

Bend has Lists and Strings, which support Unicode characters.

```rs
def main:
  return ["You: Hello, 🌎", "🌎: Hello, user"]
```

A string is desugared to a String data type containing two constructors, `String.cons` and `String.nil`.
List also becomes a type with two constructors, `List.cons` and `List.nil`.

```rs
// These two are equivalent
def StrEx:
  "Hello"

def ids:
  [1, 2, 3]

// These types are builtin.
enum String:
  String.cons(head, tail)
  String.nil
enum List:
  List.cons(head, tail)
  List.nil
def StrEx:
  String.cons('H', String.cons('e', String.cons('l', String.cons('l', String.cons('o', String.nil)))))
def ids:
  List.cons(1, List.cons(2, List.cons(3, List.nil)))
```

Characters are delimited by `'` `'` and support Unicode escape sequences. They are encoded as a U24 with the unicode codepoint as their value.

```
// These two are equivalent
def chars:
  ['A', '\u{4242}', '🌎']

def chars2:
  [65, 0x4242, 0x1F30E]
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
- &#128215; Builtin definitions: [Builtin definitions](docs/builtin-defs.md)
- &#128215; CLI arguments: [CLI arguments](docs/cli-arguments.md)
- &#128217; Duplications and superpositions: [Dups and sups](docs/dups-and-sups.md)
- &#128217; Scopeless lambdas: [Using scopeless lambdas](docs/using-scopeless-lambdas.md)
- &#128213;: Fusing functions: [Writing fusing functions](docs/writing-fusing-functions.md)

## Further reading

- &#128217; [Compilation and readback](docs/compilation-and-readback.md)
- &#128217; [Old HVM wiki learning material](https://github.com/HigherOrderCO/HVM/wiki/HVM-Wiki). It is outdated, but it can still teach you some of the basics.
