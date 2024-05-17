# Native numbers

Currently Bend supports 3 types of native numbers for fast numeric operations (compared to lambda-encoded numbers):

- U24: Unsigned integers (24 bits)
- I24: Signed integers (24 bits, two's complement)
- F24: Floating point numbers (single precision IEEE-754 floating point with the last bits of the mantissa implicitly set to zero)


### U24

Unsigned numbers are written as just the number and are represented as a 24 bit unsigned integer.

```rs
two = 2
```


### I24

Signed numbers are written with a `+` or `-` sign and are represented as a 24 bit two's complement integer.

```rs
minus_two = -2
plus_0 = +0
```

Positive numbers _must_ be written with a `+` sign, otherwise they'll be interpreted as unsigned.

Numbers can also be written in binary or hexadecimal form. Underscores can be optionally used as digit separators to make large numbers more readable.

```rs
decimal =     1194684
binary =      0b100_100_011_101_010_111_100
hexadecimal = 0x123_abc
hex_signed = -0xbeef


### F24

Floating point numbers must have the decimal point `.` and can optionally take a sign `+` or `-`.
They are represented as IEEE-754 single precision floating point numbers with the last bits of the mantissa implicitly set to zero.

```py
one = 1.0
pi = +3.1415926535897932384626433 # Will get rounded to 24bit float
a_millionth = 0.000001
zero = 0.0
minus_zero = -0.0
```


### Mixing number types

The three number types are fundamentally different.
If you mix two numbers of different types HVM will interpret the binary representation of one of them incorrectly, leading to incorrect results. Which number is interpreted incorrectly depends on the situation and shouldn't be relied on for now.

At the HVM level, both type and the operation are stored inside the number nodes as tags. One number stores the type, the other the operation.
That means that we lose the type information of one of the numbers, which causes this behavior.
During runtime, the executed numeric function depends on both the type tag and the operation tag. For example, the same tag is used for unsigned bitwise and floating point atan2, so mixing number types can give you very unexpected results.

At the moment Bend doesn't have a way to convert between the different number types, but it will be added in the future.


### Operations

There is also support for native operations.
In "Imp" syntax they are infix operators and in "Fun" syntax they are written in reverse polish notation (like you'd call a normal function).
Each operation takes two arguments and returns a new number.

```rs
# In Fun syntax
some_val = (+ (+ 7 4) (* 2 3))
```

These are the currently available operations:

Operation | Description | Accepted types | Return type
----------|-------------|----------------|------------
+         | Addition    | U24, I24, F24  | Same as arguments
-         | Subtraction | U24, I24, F24  | Same as arguments
*         | Multiplication | U24, I24, F24  | Same as arguments
/         | Division | U24, I24, F24  | Same as arguments
%         | Modulo | U24, I24, F24  | Same as arguments
==        | Equality | U24, I24, F24  | U24
!=        | Inequality | U24, I24, F24  | U24
<         | Less than | U24, I24, F24  | U24
\>        | Greater than | U24, I24, F24  | U24
&         | Bitwise and | U24, I24  | Same as arguments
|         | Bitwise or | U24, I24  | Same as arguments
^         | Bitwise xor | U24, I24  | Same as arguments
**        | Exponentiation | F24  | F24


### Pattern matching

HVM-lang also includes a `switch` syntax for pattern-matching U24 numbers.

```rs
Number.to_church = λn λf λx
  switch n {
    0: x
    _: (f (Number.to_church n-1 f x))
  }
```

The `0` case matches when `n` is 0, and the `_` case matches when `n` is greater than 0.
In the `_` arm, we can access the predecessor of `n` with the `n-1` variable.

We can also match on more than one value at once.
If we do that, we must necessarily cover the cases in order, starting from 0.

```rs
Number.minus_three = λn λf λx
  switch n {
    0: 0
    1: 0
    2: 0
    _: n-3
  }
```


Using everything we learned, we can write a program that calculates the n-th Fibonacci number using native numbers

```rs
fibonacci = λn // n is the argument
  switch n {
    // If the number is 0, then return 0
    0: 0
    // If the number is 1, then return 1
    1: 1
    // Otherwise, return the sum of (fib (n-2 + 1)) and (fib n-2)
    // The successor pattern provides a `var`-`successor number` bind
    _: (+ (fibonacci (+ n-2 1)) (fibonacci n-2))
  }

main = (fibonacci 15)
```


### Pattern matching numbers in Fun syntax equations

In Fun syntax, we can also use pattern matching equations to match on native unsigned numbers.

```rs
(fib 1) = 1
(fib 0) = 0
(fib n) = (+ (fib (- n 1)) (fib (- n 2)))
```

Unlike with `switch`, you can match any number and in any order.
The variable pattern is used to match on all other numbers.
Unlike with `switch`, you can't directly access the predecessor of the number.

You can read [Pattern matching](pattern-matching.md) for more information about how pattern matching equations are converted to `switch` and `match` expressions.