# Built-in Types and Functions

## Types

### String
```data String = (Cons head ~tail) | (Nil)```

- **Nil**: Represents an empty string.
- **Cons head ~tail**: Represents a string with a `head` character and a `tail` string.

### List
```data List = (Cons head ~tail) | (Nil)```

- **Nil**: Represents an empty list.
- **Cons head ~tail**: Represents a list with a `head` element and a `tail` list.

### Nat
```data Nat = (Succ ~pred) | (Zero)```

- **Succ ~pred**: Represents a natural number successor.
- **Zero**: Represents the natural number zero.

### Result
```data Result = (Ok val) | (Err val)```

- **Ok val**: Represents a successful result with value `val`.
- **Err val**: Represents an error with value `val`.

### Map
```data Map = (Node value ~left ~right) | (Leaf)```

- **Node value ~left ~right**: Represents a map node with a `value` and `left` and `right` subtrees.
- **Leaf**: Represents an empty map.

## Functions

### Map

#### Map/empty
Initializes an empty map.
```Map/empty = Map/Leaf```

#### Map/get
Retrieves a value from the map based on the key.
```Map/get map key = ...```

#### Map/set
Sets a value in the map at the specified key.
```Map/set map key value = ...```

## IO

### IO/Done
Represents a completed IO operation.

### IO/Call
Represents a pending IO operation.

### IO/MAGIC
Returns a magic number used internally.
```def IO/MAGIC: return (0xD0CA11, 0xFF1FF1)```

### IO/wrap
Wraps a value in an IO/Done.
```def IO/wrap(x): return IO/Done(IO/MAGIC, x)```

### IO/bind
Chains IO operations.
```def IO/bind(a, b): match a ...```

### call
Calls an IO function with an argument.
```def call(func, argm): return IO/Call(IO/MAGIC, func, argm, lambda x: IO/Done(IO/MAGIC, x))```

### print
Prints text to the console.
```print text = (IO/Call IO/MAGIC "PUT_TEXT" text @x (IO/Done IO/MAGIC x))```

# Usage Examples

## Map

### Map/empty
Initializes an empty map.
```Map/empty = Map/Leaf```


### Map/get
Retrieves a `value` from the `map` based on the `key`.

```Map/get map key =
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


### Map/set
Sets a `value` in the `map` at the specified `key`.

```Map/set map key value =
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


## IO

#IO Impl

STRING_NIL_TAG  = 0
STRING_CONS_TAG = 1

IO_DONE_TAG       = 0
IO_PUT_TEXT_TAG   = 1
IO_GET_TEXT_TAG   = 2
IO_WRITE_FILE_TAG = 3
IO_READ_FILE_TAG  = 4
IO_GET_TIME_TAG   = 5
IO_SLEEP_TAG      = 6
IO_DRAW_IMAGE_TAG = 7

data IO
  = (Done term)
  | (PutText   text      cont)
  | (GetText             cont)
  | (WriteFile file data cont)
  | (ReadFile  file      cont)
  | (GetTime             cont)
  | (Sleep     time      cont)
  | (DrawImage tree      cont)
