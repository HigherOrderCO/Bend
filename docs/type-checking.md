# Type Checking

Bend has a type checker with optional typing support based on a Hindley Milner type system.

Programs can be optionally typed using the respective imp or fun type syntax. Type checking is
enabled by default, but can be toggled with the `-Otype-check` and `-Ono-type-check` options.

Every function can be annotated with a type for its arguments and return value.
The type checker will infer the type of the function and then compare if it's compatible with the annotated type.

```python
def add(x: u24, y: u24) -> u24:
  return x + y

# Arguments or return value without annotation are considered `Any`.
# They will be accepted by any function, regardless of being correct or not.
def push(list: List(T), value) -> List(T):
  match list:
    case List/Nil:
      return List/Cons(value, List/Nil)
    case List/Cons:
      return List/Cons(list.head, push(list.tail, value))

# Error, List(T) must only receive values of type `T`.
def append_num(list: List(T), num: u24) -> List(T):
  return List/Cons(num, list)

# Error, Tree(T) can only store one type of value.
def my_tree() -> _:
  return ![!1, !"a"]

# Error, can't add a `u24` and a `f24`.
# Bend doesn't have implicit type conversions.
def add_float(x: u24, y: f24) -> f24:
  return x + y
```

Bend comes with the following builtin types:

* `u24`: Unsigned 24-bit integer.
* `i24`: Signed 24-bit integer.
* `f24`: Floating point number.
* `(T1, ..., Tn)`: Tuple with `n` elements of types `T1` to `Tn`.
* `Any`: Untyped value.
* `None`: Eraser `*`.
* `_`: A type that will be inferred by the type checker.

The prelude library also defines some basic types that are used in Bend programs:

* `String`: Text represented as a sequence of Unicode characters.
* `List(T)`: A list of values of type `T`.
* `Tree(T)`: A binary tree with values of type `T` at the leaves.
* `Map(T)`: A map from keys of type `u24` to values of type `T`.
* `IO(T)`: A monadic IO type that can be used to perform IO operations.
* `Result(O, E)`: Represents the result of an operation that can either succeed with an `O` or fail with an `E`.


Additionally, you can define your own algebraic data types.
In this case, all the type variables that occur in the constructors must be previously defined.

```python
type Option(T):
  Some { value: T }
  None
```

All the constructors will be declared with the same type `TypeName(var2, var2, ...)`.

### Enabling and disabling type checking

In some cases we know that dynamically our program will not do something wrong despite not being able to give it the proper type.

We can disable type checking for a specific function by either removing the type annotations or by giving it the `unchecked` keyword:

```python
# Error, type-checked functions can't contain an unscoped variable.
def channel(x: u24) -> (u24 -> u24, u24):
  return (lambda $a: x, $a)

# We can remove the annotations. It won't be type-checked,
# but its type will be `Any -> Any`.
def channel(x):
  return (lambda $a: x, $a)

# Instead, we can use the `unchecked` keyword.
# The annotated type will be considered the truth, regardless of being correct or not.
def unchecked channel(x: u24) -> (u24 -> u24, u24):
  return (lambda $a: x, $a)
```

The opposite is also possible, we can enable type checking for an unannotated function by using the `checked` keyword before the name of the function in its declaration:

```python
# Despite the inferred type being `List(T) -> List(T)`, the type checker will consider it as `Any -> Any` because it's not annotated.
def checked tail(list):
  match list:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return list.tail

# Error, can't infer the type of this function, despite having type `Any`.
# Not typeable by a Hindley-Milner type system.
checked (scott_concat a b) = (a
  λh λt λcons λnil (cons h (scott_concat t b))
  b
)
```

We can also disable type checking for the entire program by using the `-Ono-type-check` option.

Native HVM definitions are always unchecked.

```python
# This function will be given the type `a -> a`.
hvm native_id -> (a -> a):
  (x x)
```

### Limitations

Currently, the following are not supported by the type checker:

- Superpositions (`{a, b}`, the tuple type with duplication semantics, see [Dups and sups](https://github.com/HigherOrderCO/Bend/blob/main/docs/dups-and-sups.md)).
- Unscoped variables and variable binds (`$a`, `let $a = ...`, see [Scopeless lambdas](https://github.com/HigherOrderCO/Bend/blob/main/docs/using-scopeless-lambdas.md)).
- Expressions not typeable by a Hindley-Milner type system (e.g. self application `λx: x(x)`).

Additionally, the builtin types `Number` and `Integer` can't be used directly in type annotations. They are used internally by the type checker to handle numeric expressions.

```python
# The inferred type will be `Number(a) -> Number(a) -> Number(a)`.
def add(x: _, y: _) -> _:
  return x + y

# The inferred type will be `Integer(a) -> Integer(a) -> Integer(a)`.
def shift(x: _, n: _) -> _:
  return x << n
```
