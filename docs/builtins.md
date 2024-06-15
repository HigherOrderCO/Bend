>this is a WIP based on [Builtins.bend](https://github.com/HigherOrderCO/Bend/blob/main/src/fun/builtins.bend).

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

## Map
```python
type Map:
  Node { value ~left ~right }
  Leaf
```

**`Map`** represents a tree with values stored in the branches.
It is meant to be used as an efficient map data structure with integer keys and O(log n) read and write operations.

- **Node { value ~left ~right }**: Represents a map node with a `value` and `left` and `right` subtrees. Empty nodes have `*` stored in the `value` field.
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
Sets a `value` in the `map` at the specified `key`.
Returns the map with the new value.
```rust
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
```

#### Syntax
With the same map that we `set` in the previous section, we can map it's values with `@=`:
```py
x[0] @= lambda y: String/concat(y, " and mapped")
# x[0] now contains "swapped and mapped"
```


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



## IO
IO Functions are in the **next milestone**!

## Numeric operations

### log
```py
def log(x: f24, base: f24) -> f24
```
Computes the logarithm of `x` with the specified `base`.

### atan2
```py
def atan2(x: f24, y: f24) -> f24
```
Computes the arctangent of `y / x`.

Has the same behaviour as `atan2f` in the C math lib.
