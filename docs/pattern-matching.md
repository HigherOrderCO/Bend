# Pattern Matching

Switches on many numbers are compiled to sequences of simple switch expressions:
```py
  # These two are equivalent
  switch n {
    0: A
    1: B
    2: C
    _: (D n-3)
  }

  switch n {
    0: A
    _: switch n-1 = n-1 {
      0: B
      _: switch n-2 = n-1-1 {
        0: C
        _: use n-3 = n-2-1; (D n-3)
      }
    }
  }
```

Matches on ADT constructors are compiled to different expressions depending on the chosen encoding:
```py
type Maybe = (Some val) | None

UnwrapOrZero x = match x {
  Some: x.val
  None: 0
}

# If the current encoding is 'adt-num-scott' it becomes:
Some = λval λx (x 0 val)
None = λx (x 1)
UnwrapOrZero x = (x λtag switch tag {
  0: λx.val x.val
  _: λ* 0
})

# Otherwise, if the current encoding is 'adt-scott' it becomes:
Some = λval λSome λNone (Some val)
None = λSome λNone None
UnwrapOrZero x = (x λx.val x.val 0)
```

### Pattern Matching functions

Besides `match`and `switch` terms, hvm-lang also supports equational-style pattern matching functions.

```py
And True  b = b
And False * = False
```

There are advantages and disadvantages to using this syntax.
They offer more advanced pattern matching capabilities and also take care linearizing variables to make sure that recursive definitions work correctly in strict evaluation mode, but take away your control of how the pattern matching is implemented and can be a bit more resource intensive in some cases.

Pattern matching equations are transformed into a tree of `match` and `switch` terms from left to right.
```py
# These two are equivalent
(Foo 0 false (Cons h1 (Cons h2 t))) = (A h1 h2 t)
(Foo 0 * *) = B
(Foo n false *) = n
(Foo * true *) = 0

Foo = λarg1 λarg2 λarg3 (switch arg1 {
  0: λarg2 λarg3 match arg2 {
    true: λarg3 B
    false: λarg3 match arg3 {
      Cons: (match arg3.tail {
        Cons: λarg3.head (A arg3.head arg3.tail.head arg3.tail.tail)
        Nil: λarg3.head B
      } arg3.head)
      Nil: B
    }
  }
  _: λarg2 λarg3 (match arg2 {
    true: λarg1-1 0
    false: λarg1-1 (+ arg1-1 0)
  } arg1-1)
} arg2 arg3)
```
Besides the compilation of complex pattern matching into simple `match` and `switch` expressions, this example also shows how some arguments are pushed inside the match.
When compiling for strict evaluation, by default any variables that are used inside a match get linearized by adding a lambda in each arm and an application passing its value inwards.
To ensure that recursive pattern matching functions don't loop in strict mode, it's necessary to make the match arms combinators, so that they can be converted into separate functions and a lazy reference is used in the match arm.
```py
# This is what the Foo function actually compiles to.
# With -Olinearize-matches and -Ofloat-combinators (default on strict mode)
(Foo) = λa λb λc (switch a { 0: Foo$C5; _: Foo$C8 } b c)

(Foo$C5) = λd λe (d Foo$C0 Foo$C4 e) # Foo.case_0
(Foo$C0) = λ* B                      # Foo.case_0.case_true
(Foo$C4) = λg (g Foo$C3 B)           # Foo.case_0.case_false
(Foo$C3) = λh λi (i Foo$C1 Foo$C2 h) # Foo.case_0.case_false.case_cons
(Foo$C1) = λj λk λl (A l j k)        # Foo.case_0.case_false.case_cons.case_cons
(Foo$C2) = λ* B                      # Foo.case_0.case_false.case_cons.case_nil

(Foo$C8) = λn λo λ* (o Foo$C6 Foo$C7 n) # Foo.case_+
(Foo$C6) = λ* 0                         # Foo.case_+.case_true
(Foo$C7) = λr (+ r 1)                   # Foo.case_+.case_false
```

Pattern matching equations also support matching on non-consecutive numbers:
```rust
Parse '(' = Token.LParenthesis
Parse ')' = Token.RParenthesis
Parse 'λ' = Token.Lambda
Parse  n  = (Token.Name n)
```
This is compiled to a cascade of `switch` expressions, from smallest value to largest.
```py
Parse = λarg0 switch matched = (- arg0 '(') {
  0: Token.LParenthesis
  # ')' + 1 - '(' is resolved during compile time
  _: switch matched = (- matched-1 ( ')'-1-'(' ) {
    0: Token.RParenthesis
    _: switch matched = (- matched-1 ( 'λ'-1-')' ) {
      0: Token.Lambda
      _: use n = (+ 1 matched-1); (Token.Name n)
    }
  }
}
```
Unlike with `switch`, with pattern matching equations you can't access the value of the predecessor of the matched value directly, but instead you can match on a variable.
Notice how in the example above, `n` is bound to `(+ 1 matched-1)`.

Notice that this definition is valid, since `*` will cover both `p` and `0` cases when the first argument is `False`.

```rust
pred_if False * if_false = if_false
pred_if True  p *        = (- p 1)
pred_if True  0 *        = 0
```

Pattern matching on strings and lists desugars to a list of matches on List/String.cons and List/String.nil

```py
Hi "hi" = 1
Hi _ = 0

Foo [] = 0
Foo [x] = x
Foo _ = 3

# Becomes:
Hi (String.cons 'h' (String.cons 'i' String.nil)) = 2
Hi _ = 0

Foo List.nil = 0
Foo (List.cons x List.nil) = x
Foo _ = 3
```
