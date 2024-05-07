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

Each constructor is defined by a name followed by its fields. The `~` notation describes a recursive field.

## Statements

### Assignment

```python
value = 2
return value

(first, second) = (1, 2)
return second
```

Assigns a value to a variable, it's possible to pattern match match tuples.

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

Returns the following expression.

### If

```python
if x:
  return 0
else:
  return 1
```

A branching statement where `else` is mandatory.

### Switch

```python
switch x = 4:
  0: "Zero"
  1: "One"
  _: "Not zero or one"
```

A switch for native numbers, the pattern matching cases must start from `0` up to `_` sequentially.

It is possible to bind a variable name to the matching value.

### Match

```python
match x = Option/none:
  Option/some:
    return x.value
  Option/none:
    return 0
```

A pattern matching statement, the cases must be the constructor names of the matching value.

It is possible to bind a variable name to the matching value.

### Fold

```python
fold x = Tree/leaf:
  Tree/node:
    return x.value + x.left + x.right
  Tree/leaf:
    return 0
```

A fold statement. Reduces the given value with the given match cases.

It is possible to bind a variable name to the matching value.

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

### Lambdas

```python
lambda x, y: y
```

A lambda abstraction, it can bind one or more variables and has an expression as body.

### Call

```python
callee(arg1, arg2, argn)
```

A call is written with a callee followed by a list of arguments.

### Tuple

```python
(3, 9)
```

A Tuple is surrounded by `(` `)`, it's elements are separated by `,`.

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

A Character is surrounded with `'`. It is desugared as a number.

### String Literal

```python
"Hello, World!"
```

A String literal is surrounded with `"`.

### List Literal

```python
[1, 2, "three"]
```

A List literal is surrounded by `[` `]`. The elements must be separated by `,`.

### List Comprehension

```python
[x + 1 for x in list]

[x + 1 for x in list if x > 2]
```

A List Comprehension generates a new list, it can be extracted in 3 parts.

`[expression . iterator . condition]`

Expression: The expression to be performed in the iterator element.

Iterator: Binds a name to the list elements.

Condition: Optional, is applied after the list is processed.

<div id="core-syntax"></div>

# Core Syntax

## Top-level definitions

```rust
data Name
  = (Ctr1 arg1 arg2)
  | Ctr2

Name (Ctr1 subarg1 subarg2) arg3 = rule0_body
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

A function definition is composed of a sequence of rules, where a rule is the name of the function, a sequence of patterns and then the body.

A rule pattern can be:
- A variable.
- A constructor.
- A tuple.
- A superposition.

The rule body is a `term`, there is no statements in the language.

### Datatype

Defines an Algebraic Data Type, it should have at least one constructor.

```rust
data Tree
  = (Leaf value)
  | (Node left right)
  | Nil
```

`Tree` is the ADT name and it should be unique, except that it can be used once by a constructor name.

Each constructor is defined by a name followed by its fields.

## Terms

### Lambda

```rust
@x x

λx x

λ(fst, snd) snd

λ{x y} x
```

Lambdas represents functions, it can be written with `λ` or `@` followed by a pattern and a term.

A pattern is equivalent to a let:

```rust
λa let (fst, snd) = a; snd

λa let {x y} = a; x
```

### Unscoped Lambdas and Variables

```
λ$x $x
```

Same as lambdas, with the exception that the variable starts with a `$` sign.

Read [using scopeless lambdas](/docs/using-scopeless-lambdas.md) to know more about.

### Application

```rust
(fun argn)
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

It is possible to use a _wildcard_ variable or `*` as an exhaustive default case.

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

A Character literal is surrounded with `'`. It is desugared as an u24 number.

### String Literal

```rust
"Hello"
```

A String literal is surrounded with `"`.

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
