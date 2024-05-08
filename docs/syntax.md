# Syntax

This file provides a reference of each possible syntax of bend programming language.

Click [here](#bend-syntax) to see the bend syntax.

Click [here](#core-syntax) to see the core syntax.

<div id="bend-syntax"></div>

# Bend Syntax

## Top-level definitions

### Def

Defines a top level function.

```python
def add(x, y):
  result = x + y
  return result

def main:
  return add(40, 2)
```

A definition is composed by a name, a sequence of parameters and a body.

A top-level name can be anything matching the regex `[A-Za-z0-9_.-/]+`, except it can't have `__` (used for generated names).

### Enum

Defines an ADT like enumeration.

```python
enum Option:
  Some(value)
  None

enum Tree:
  Node(value, ~left, ~right)
  Leaf
```

Enum names must be unique, and should have at least one constructor.

Each constructor is defined by a name followed by its fields.

The `~` notation indicates a recursive field. To use `fold` statements with a type its recursive fields must be correctly marked with `~`.

The constructor names inherit the name of their types and become functions (`Tree/Node` and `Tree/Leaf` in this case).
The exact function they become depends on the encoding.

Read [defining data types](./defining-data-types.md) to know more.

## Statements

### Assignment

```python
value = 2
return value

(first, second) = (1, 2)
return second

{x y} = {2 3}
```

Assigns a value to a variable, it's possible to pattern match match tuples and superpositions.

### In-Place Operation

```python
x += 1
return x
```

The in-place operation does an infix operation and re-assigns a variable.

The operations are:
- Addition `+=`
- Subtraction `-=`
- Multiplication `*=`
- Division `/=`

### Return

```python
return "hello"
```

Returns the following expression. All paths or branches should end with a return.

### If

```python
if condition:
  return 0
else:
  return 1
```

A branching statement where `else` is mandatory.

The condition must return an `u24` and it is equivalent to a switch statement:

```
switch _ = condition:
  0: else
  _: then
```

### Switch

```python
switch x = 4:
  0: "Zero"
  1: "One"
  _: "Not zero or one"
```

A switch for native numbers, the pattern matching cases must start from `0` up to `_` sequentially.

It is possible to bind a variable name to the matching value, it allows the access to the predecessor `x-2` (in this case) or `bound_var-next_num` (in the general case).

### Match

```python
match x = Option/none:
  Option/some:
    return x.value
  Option/none:
    return 0
```

A pattern matching statement, the cases must be the constructor names of the matching value.

It is possible to bind a variable name to the matching value. The fields of the matched constructor are bound to `matched_var.field_name`.

### Fold

```python
fold x = Tree/leaf:
  Tree/node:
    return x.value + x.left + x.right
  Tree/leaf:
    return 0
```

A fold statement. Reduces the given value with the given match cases.

It is possible to bind a variable name to the matching value. Just like in `match`, the fields are bound to `matched_var.field_name`.

For fields notated with `~` in the type definition, the fold function is called implicitly.

It is equivalent to the inline recursive function:

```python
def fold(x):
  match x:
    Tree/Node:
      return x.value + fold(x.left) + fold(x.right)
    Tree/Leaf:
      return 0
...
fold(Tree/Leaf)
```

### Bend

Bend can be used to create recursive data structures:

```rust
bend x = 0 while x < 10:
  left = go(x + 1)
  right = go(x + 1)
  result = Tree/Node(left, right)
then:
  result = Tree/Leaf(x)
```

Which binds a variable to the return of an inline recursive function.
The function `go` is available inside the bend body and calls it recursively.

It is possible to initialize multiple variables:

```python
bend x = 1, y = 2 ... while condition(x, y, ...):
```

It is equivalent to this inline recursive function:

```python
def bend(x, y, ...):
  if condition(x, y, ...):
    ...
    return ... bend(x, y, ...) ...
  else:
    return ...
```

### Do

```python
do Result.bind:
  x <- safe_div(2, 0)
  return x
```

A monadic do block.

Where `x <- ...` performs a monadic operation.

Other statements are allowed inside the `do` block.

## Expressions

### Variables

```python
some_var

foo/bar
```

A variable can be anything matching the regex `[A-Za-z0-9_.-/]+`.

A variable is a name for some immutable expression. It is possible to rebind variables with the same name.

```python
x = 1
x = x + 1
```

### Lambdas

```python
x => x

x => y => y
```

Lambdas represents anonymous inline functions, it can bind a variable and has an expression as body.

### Unscoped Lambdas and Variables

```python
$x => $x
```

Like lambdas, with the exception that the variable starts with a `$` sign. Every unscoped variable in a function must have a unique name and must be used exactly once.

Unscoped variables are not transformed and linearized like normal scoped variables.

Read [using scopeless lambdas](/docs/using-scopeless-lambdas.md) to know more about.

### Function Call

```python
callee(arg_1, arg_2, arg_n)
```

A call is written with a callee followed by a list of arguments.

The effect of a function call is to substitute the callee with it's body and replace the arguments by the passed variables.

Accepts partial applications.

### Tuple

```python
(3, 9)
```

A Tuple is surrounded by `(` `)` and should contain 2 or more elements. Elements are separated by `,`.

### Superposition

```python
{1 2 3}
```

A superposition of values is defined using `{` `}` with at least 2 expressions inside.

Read [sups and dups](./dups-and-sups.md) to know more.

### Numbers and Infix Operations

Currently, bend supports 3 types of numbers: floats, integers and unsigned integers. All of then are 24 bit sized.

```python
f24 = +88.012

i24 = -42

u24 = 42
```

| Operation      | Syntax |
|----------------|--------|
| Addition       | x + y  |
| Subtraction    | x - y  |
| Multiplication | x * y  |
| Division       | x / y  |
| Equal          | x == y |
| Not Equal      | x != y |
| Less Than      | x < y  |
| Greater Than   | x > y  |
| Bitwise And    | x & y  |
| Bitwise Or     | x | y  |
| Bitwise Xor    | x ^ y  |

### Character Literal

```python
'x'
```

A Character is surrounded with `'`. Accepts unicode characters, unicode escapes in the form '\u{hex value}' and is desugared to the unicode codepoint as an `u24`.

Only supports unicode codepoints up to `0xFFFFFF`.

### String Literal

```python
"Hello, World!"
```

A String literal is surrounded with `"`. Accepts the same values as characters literals.

It is desugared to constructor calls of the built-in type String, `String/cons(head, ~tail)` and `String/nil` .

### List Literal

```python
[1, 2, "three"]
```

A List literal is surrounded by `[` `]`. The elements must be separated by `,`.

It is desugared to constructor calls of the built-in type List, `List/cons(head, ~tail)` and `List/nil` .

### List Comprehension

```python
[x + 1 for x in list]

[x + 1 for x in list if x > 2]
```

A List Comprehension generates a new list, it can be extracted in 3 parts.

`[expression . iterator . condition]`

Expression: The expression to be performed in the iterator element.

Iterator: Binds a name to the list elements.

Condition: Optional, is used to filter the list elements.

It is desugared to a fold statement:

```python
fold list:
  List/cons:
    if condition:
      List/cons(list.head, list.tail)
    else:
      list.tail
  List/nil:
    List/nil
```

<div id="core-syntax"></div>

# Core Syntax

## Top-level definitions

```rust
data Name
  = (Ctr1 arg1 arg2)
  | Ctr2

Name (Ctr1 sub_arg1 sub_arg2) arg3 = rule0_body
Name Ctr2 arg3 = rule1_body
```

### Definitions

Definitions can have multiple rules pattern matching each defined argument.

```rust
identity x = x

Bool.neg True  = False
Bool.neg False = True

MapMaybe (Some val) f = (Some (f val))
MapMaybe None f = None

Pair.get (fst, snd) f = (f fst snd)
```

A top-level name can be anything matching the regex `[A-Za-z0-9_.-/]+`, except it can't have `__` (used for generated names).

A function definition is composed of a sequence of rules, where a rule is the name of the function, a sequence of patterns and then the body.

A rule pattern can be:
- A variable.
- A number.
- A constructor.
- A tuple.
- A superposition.

The rule body is a `term`, there is no statements in the language.

Read [pattern matching](./pattern-matching.md) to learn about what exactly the rules for pattern matching equations are.

### Datatype

Defines an Algebraic Data Type, it should have at least one constructor.

```rust
data Tree
  = (Leaf value)
  | (Node ~left ~right)
  | Nil
```

`Tree` is the ADT name and it should be unique, except that it can be used once by a constructor name.

Each constructor is defined by a name followed by its fields. The `~` notation describes a recursive field.

## Terms

### Variables

A variable can be anything matching the regex `[A-Za-z0-9_.-/]+`.

A variable is a name for some immutable expression. It is possible to rebind variables with the same name.

```rust
let x = 1
let x = (+ x 1)
```

### Lambda

```rust
@x x

λx x

λ(fst, snd) snd

λ{x y} x
```

Lambdas represents anonymous inline functions, it can be written with `λ` or `@` followed by a pattern and a term.

A pattern is equivalent to a let:

```rust
λa let (fst, snd) = a; snd

λa let {x y} = a; x
```

### Unscoped Lambdas and Variables

```
λ$x $x
```

Same as lambdas, with the exception that the variable starts with a `$` sign. Every unscoped variable in a function must have a unique name and must be used exactly once.

Unscoped variables are not transformed and linearized like normal scoped variables.

Read [using scopeless lambdas](/docs/using-scopeless-lambdas.md) to know more about.

### Application

```rust
(fun arg_1 arg_2 ... arg_n)
```

An application is surrounded by `(` `)`, written in lisp style.

> Lambdas have a higher precedence, so `(@x x 1)` and `((@x x) 1)` means the same thing.

### Tuples

```rust
(1, 2, 3)
```

A tuple is surrounded by `(` `)`, with the difference that it's elements are separated by `,`.

### Superposition

```rust
{1 2 3}
```

A superposition of values is defined using `{` `}` with at least 2 terms inside.

Read [sups and dups](./dups-and-sups.md) to know more.

### Let-bindings

```rust
let x = (+ 1 2)
x

let (fst, snd, era) = (1, 2, *);
(+ fst snd)

let {f1 f2} = λx x;
(f1 f2)

let $x = (some_fn $x);
*
```

> `*` is an eraser term.

A let term uses a pattern, it can be:
- A variable / unscoped variable.
- A tuple.
- A superposition.

The let term will expects a binding value followed by a `next` term.

Using `;` is optional.

### Switch

```rust
switch n {
  0: "zero"
  1: "one"
  _: "greater than 1"
}

switch x = (+ 1 1) {
  0: 42;
  _: x-1;
}
```

A switch for native numbers, it can hold a name binding if the matching term is not a variable.

The cases need to be typed from `0` to a wildcard `_` in sequence.

Using `;` is optional.

### Match

```rust
match opt = (Some "Bend") {
  Some: opt.value;
  None: "No name";
}
```

A pattern match expression, it can hold a name binding if the matching term is not a variable.

It is possible to use a _wildcard_, a named variable or `*` as default cases.

It is desugared according to the chosen encoding. Read [pattern matching](./pattern-matching.md) to know more.

Using `;` is optional.

### Monadic bind

```rust
Result.bind (Result.ok val) f = (f val)
Result.bind err _ = err

div a b = switch b {
  0: (Result.err "Div by 0")
  _: (Result.ok (/ a b))
}

rem a b = switch b {
  0: (Result.err "Mod by 0")
  _: (Result.ok (% a b))
}

Main = do Result.bind {
  ask y = (div 3 2);
  ask x = (rem y 0);
  x
}
```

Receives a monadic bind function and then expects a block.

The block is followed by `ask` binds and ends with a return term.

The monad bind function should be of type `(Monad a) -> (a -> (Monad b)) -> (Monad b)`.

### Numbers and operations

Currently, bend supports 3 types of numbers: floats, integers and unsigned integers. All of then are 24 bit sized.

```rust
f24 = +88.012

i24 = -42

u24 = 42
```

|   Operation    |  Syntax  |
|----------------|----------|
| Addition       | (+ x y)  |
| Subtraction    | (- x y)  |
| Multiplication | (* x y)  |
| Division       | (/ x y)  |
| Remainder      | (% x y)  |
| Equal          | (== x y) |
| Not Equal      | (!= x y) |
| Less Than      | (< x y)  |
| Greater Than   | (> x y)  |
| Bitwise And    | (& x y)  |
| Bitwise Or     | (\| x y) |
| Bitwise Xor    | (^ x y)  |

### Character Literal

```rust
'a'
```

A Character is surrounded with `'`. Accepts unicode characters, unicode escapes in the form '\u{hex value}' and is desugared to the unicode codepoint as an `u24`.

Only supports unicode codepoints up to `0xFFFFFF`.

### String Literal

```rust
"Hello"
```

A String literal is surrounded with `"`. Accepts the same values as characters literals.

The syntax above is desugared to:

```
(String.cons 'H' (String.cons 'e' (String.cons 'l' (String.cons 'l' (String.cons 'o' String.nil)))))
```

### List Literal

```rust
[1, 2, 3 4]
```

The syntax above is desugared to:

```
(List.cons 1 (List.cons 2 (List.cons 3 (List.cons 4 List.nil))))
```

Using `,` is optional.

### Nat Literal

```rust
#3
```

The syntax above is desugared to:

```
(Nat.succ (Nat.succ (Nat.succ List.nil)))
```
