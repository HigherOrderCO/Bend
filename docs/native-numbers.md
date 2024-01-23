# Native numbers

HVM is more than a pure functional programming language. HVM also supports native unsigned 60-bits integers to improve performance.

```rs
two = 2
```

Native integers should be used instead of Scott-encoded natural numbers when performance is needed.

Numbers can also be written in binary or hexadecimal form. Underscores can be optionally used as digit separators to make large numbers more readable.

```rs
decimal =     1194684
binary =      0b100_100_011_101_010_111_100
hexadecimal = 0x123_abc
```

There is also support for native operations. They are written in reverse polish notation and take 2 arguments each.

```rs
some_val = (+ (+ 7 4) (* 2 3))
```
The current operations include `+, -, *, /, %, ==, !=, <, >, <=, >=, &, |, ^, ~, <<, >>`.

The `~` symbol stands for NOT. It takes two arguments and calculates the 60-bit binary NOT of the first one, but ignores its second one. However, because of implementation details, the second argument must be a number too.

```rs
main = (~ 42 10)
// Outputs the same thing as (~ 42 50)
// And as (~ 42 1729)
// But not the same thing as (~ 42 *) (!)
```

HVM-lang also includes a `match` syntax for native numbers. The `0` case is chosen when `n` is 0, and the `+` case is chosen when `n` is greater than 0. The previous number, by default, bound to `n-1`.
```rs
Number.to_church = λn λf λx 
  match n {
    0: x
    +: (f (Number.to_church n-1 f x))
  }
// Alternative syntax
Number.to_church = λn λf λx 
  match n {
    0: x
    +p: (f (Number.to_church p f x))
  }
// Alternative syntax with name binding
Number.to_church = λn λf λx 
  match num = n {
    0: x
    +: (f (Number.to_church num-1 f x)
  }
```

Using everything we learned, we can write a program that calculates the n-th Fibonacci number using native numbers

```rs
fibonacci = λn // n is the argument
  match n {
    // If the number is 0, then return 0
    0: 0
    // If the number is greater than 0, bind it predecessor to `a`
    +a:
    match a {
      // If the predecessor is 0, then return 1
      0: 1
      // Otherwise, bind n-2 to `b` and return the sum of (fib n-1) and (fib n-2)
      +b: (+ (fibonacci a) (fibonacci b))
    }
  }

main = (fibonacci 15)
```
