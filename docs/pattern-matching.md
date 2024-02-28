# Pattern Matching

HVM-Lang offers pattern matching capabilities. You can use pattern matching in function definitions and with the `match` expression.

Pattern matching definitions are just a syntax sugar to match expressions:

```rust
// These two are equivalent
(Foo 0 false (Cons h1 (Cons h2 t))) = (A h1 h2 t)
(Foo 0 * *) = B
(Foo 1+n false *) = n
(Foo 1+n true *) = 0

Foo = λarg1 λarg2 λarg3 match arg1 arg2 arg3 {
  (Foo 0 false (Cons h1 (Cons h2 t))): (A h1 h2 t)
  (Foo 0 * *): B
  (Foo 1+n false *): n
  (Foo 1+n true *): 0
}
```

Pattern matching on numbers has two forms.
With the successor pattern `n+var` it will expect to cover every case up to `n`:

```rust
// Error: Case '2' not covered.
match n {
  1: B;
  0: A;
  3+: ...;
}

match n {
  0: A
  1: B
  ...
  n+var: (N var)
}

// Becomes:
match n {
  0: A
  1+p: match p {
    ...
    1+var: (N var)
  }
}
```

With the wildcard pattern you can use any number freely:

```rust
match n {
  23: A
  6343: B
  0: C
  ...
  var: (N var)
}

// Becomes:
match (- n 32) {
  0: A
  1+n:
    let n = (+ (+ n 1) 23); match (- n 6453) {
    ...
    1+var: let n = (+ (+ n 1) N-1); (N var)
  }
}
```

Notice that this definition is valid, since `*` will cover both `1+p` and `0` cases when the first argument is `False`.

```rust
pred_if False *   if_false = if_false
pred_if True  1+p *        = p
pred_if True  0   *        = 0
```

Match on tuples become let tuple destructors, which are compiled to a native tuple destructor in hvm-core.

```rust
match x {
  (f, s): s
}

// Becomes:
let (f, s) = x; s
```

Match on vars becomes a rebinding of the variable with a let expression.

```rust
match x {
  c: (Some c)
}

// Becomes:
let c = x; (Some c)
```

Pattern matching on strings and lists desugars to a list of matches on List/String.cons and List/String.nil

```rust
Hi "hi" = 1
Hi _ = 0

Foo [] = 0
Foo [x] = x
Foo _ = 3

// Becomes:
Hi (String.cons 'h' (String.cons 'i' String.nil)) = 2
Hi _ = 0

Foo List.nil = 0
Foo (List.cons x List.nil) = x
Foo _ = 3
```

Matches on ADT constructors are compiled to different expressions depending on the chosen encoding.

```rust
data Maybe = (Some val) | None

match x {
  (Some val): val
  None: 0
}

// If the current encoding is 'adt-tagged-scott' it becomes:
#Maybe (x #Maybe.Some.val λval val 0)

// Otherwise, if the current encoding is 'adt-scott' it becomes:
(x λval val 0)
```

If constructor fields are not specified, we implicitly bind them based on the name given in the ADT declaration.

```rust
data Maybe = (Some value) | None

match x {
  Some: x.value
  None: 0
}

// Becomes:
match x {
  (Some x.value): x.value
  (None): 0
}
```

Nested pattern matching allows for matching and deconstructing data within complex structures.

```rust
data Tree = (Leaf value) | (Node left right)

match tree {
  (Node (Leaf a) (Leaf b)): (Combine a b)
  (Node left right): (Merge left right)
  Leaf: (Single tree.value)
}

// Which is roughly equivalent to:
match tree {
  (Node left right): match left {
    (Node left.left left.right):
      let left = (Node left.left left.right);
      (Merge left right)
    (Leaf a): match right {
      (Node right.left right.right):
        let left = (Leaf a);
        let right = (Node right.left right.right);
        (Merge left right)
      (Leaf b): (Combine a b)
    }
  }
  (Leaf value): (Single value)
}
```
