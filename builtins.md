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
