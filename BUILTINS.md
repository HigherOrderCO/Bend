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



## Result
```python
type Result = (Ok val) | (Err val)
```

- **Ok val**: Represents a successful result with value `val`.
- **Err val**: Represents an error with value `val`.

## Map
```python
type Map = (Node value ~left ~right) | (Leaf)
```

- **Node value ~left ~right**: Represents a map node with a `value` and `left` and `right` subtrees.
- **Leaf**: Represents an empty map.

#### Syntax
**Bend** has a built-in binary tree map data structure where the key is a `u24`, meaning you can use numbers, characters, and symbols as keys.
```python
{ 0: 4, `hi`: "bye", 'c': 2 + 3 }
```


### Map/empty
Initializes an empty map.
```python
Map/empty = Map/Leaf
```

### Map/get
Retrieves a `value` from the `map` based on the `key`.
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
Considering the following tree
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
```
x[0] = "swapped"     # Assigns the key 0 to the value "swapped"
```
And the value resultant from the get function would be:
```
{ 0: "swapped", 1: "bye", 2: "maybe", 3: "yes"}
```
If there's no matching `key` in the tree, it would add a new branch to that tree with the value `set`

```
x[4] = "added"     # Assigns the key 4 to the value "added"
```
The new tree
```
{ 0: "swapped", 1: "bye", 2: "maybe", 3: "yes", 4: "added"}
```

## IO
IO Functions are in the **next milestone**!
