# HVM-Lang

HVM-Lang serves as an Intermediate Representation for HVM-Core, offering a higher level syntax for writing programs based on the [Interaction-Calculus](https://github.com/VictorTaelin/Interaction-Calculus#interaction-calculus).

## Installation

With the nightly version of rust installed, clone the repository:
```bash
git clone https://github.com/HigherOrderCO/hvm-lang.git

cd hvm-lang
```

Install using cargo:
```bash
cargo install --path .
```

## Hello World!

First things first, let's write a basic program that adds the numbers 3 and 2.

```hs
main = (+ 3 2)
```

HVM-Lang searches for the `main | Main` definitions as entrypoint of the program.

To run a program, use the `run` argument:
```bash
hvml run <file>
```

It will show the number 5.
Adding the `--stats` option displays some runtime stats like time and rewrites.

To limit the runtime memory, use the `--mem <size> option.` The default is 1GB:
```bash
hvml --mem 65536 run <file>
```
You can specify the memory size in bytes (default), kilobytes (k), megabytes (m), or gigabytes (g), e.g., `--mem 200m.`

To compile a program use the `compile` argument:
```bash
hvml compile <file>
```
This will output the compiled file to stdout.

## Language Syntax

HVM-Lang syntax consists in Terms and Definitions.
A Term represents a value, such as a Number, an Application, Function, etc. A Definition points to a Term.

Here we are defining 'two' as the number 2:
```rs
two = 2
```

Numbers can also be written as binary or hexadecimal by using the prefix `0b` and `0x` respectively.  
You can use `_` as a digit separator, it can be used on large numbers to make them more readable:
```rs
decimal =     1194684
binary =      0b100_100_011_101_010_111_100
hexadecimal = 0x123_abc
```

Currently, the only supported type of machine numbers are unsigned 60-bit integers.  

A lambda where the body is the variable `x`:
```rs
id = λx x
```

Operations can handle just 2 terms at time:
```rs
some_val = (+ (+ 7 4) (* 2 3))
```
The current operations include `+, -, *, /, %, ==, !=, <, >, <=, >=, &, |, ^, ~, <<, >>`.

A let term binds some value to the next term, in this case `(* result 2)`:
```rs
let result = (+ 1 2); (* result 2)
```

It is possible to define tuples:
```rs
tup = (2, 2)
```

And destructuring tuples with `let`:
```rs
let (x, y) = tup; (+ x y)
```

Term duplication is done automatically when a variable is used more than once. But it's possible to manually duplicate a term using `dup`:
```rs
// the number 2 in church encoding using dup.
ch2 = λf λx dup f1 f2 = f; (f1 (f2 x))

// the number 3 in church encoding using dup.
ch3 = λf λx dup f0 f1 = f; dup f2 f3 = f0; (f1 (f2 (f3 x)))
```

A `sup` is a superposition of two values, it is defined using curly brackets with two terms inside
```rs
sup = {3 7}
```

Sups can be used anywhere a value is expected, if anything interacts with the superposition, the result is the superposition of that interaction on both the possible values:

```rs
mul = λa λb (* a b)
result     = (mul 2 5)         // returns 10
result_sup = (mul 2 {5 7})     // returns {10 14}
multi_sup  = (mul {2 3} {5 7}) // returns {{10 14} {15 21}}
```

To access both values of a superposition, `dups` with labels are needed.  
A `dup` generally just duplicates the term it points to:

```rs
// each dup variable now has a copy of the {1 2} superposition
dup x1 x2 = {1 2}
```

Both `dups` and `sups` support labels, that is, a field starting with `#` to identify their counterpart:
```rs
// here, x1 now contains the value of 1, and x2 the value of 2
dup #i x1 x2 = {#i 1 2}
```

Due to how dups are compiled, dup tags between two interacting terms should not contain the same label. For example, an application of the church numeral 2 with itself:

```rs
c2 = λf λx dup f1 f2 = f; (f1 (f2 x))
main = (c2 c2)
```

To avoid label collision, HVM-Lang automatically generates new dup labels for each dup in the code. But with cases like the example above, when the interacting dups comes from the same place, the result is an invalid reduction.  

To fix the problem, its necessary to re-create the term so that a new label is assigned, or manually assign one:
```rs
c2  = λf λx dup        f1 f2 = f; (f1 (f2 x))
c2_ = λf λx dup #label f1 f2 = f; (f1 (f2 x))
main = (c2 c2_)
```

It is possible to use channels, the channel variable can occur outside it's body:
```rs
($a (λ$a 1 λb b))
```
This term will reduce to:
```
(λb b 1)
1
```

A match syntax for machine numbers.
We match the case 0 and the case where the number is greater
than 0, if `n` is the matched variable, `n-1` binds the value of the number - 1:
```rs
Number.to_church = λn λf λx 
  match n {
    0: x
    +: (f (Number.to_church n-1 f x))
  }
```

It is possible to define Data types using `data`.  
If a constructor has any arguments, parenthesis are necessary around it:
```rs
data Option = (Some val) | None
```

If the data type has a single constructor, it can be destructured using `let`:
```rs
data Boxed = (Box val)

let (Box value) = boxed; value
```

Otherwise, there are two pattern syntaxes for matching on data types.  
One which binds implicitly the matched variable name plus `.` and the fields names on each constructor:

```rs
Option.map = λoption λf
  match option {
    Some: (Some (f option.val))
    None: None
  }
```

And another one which deconstructs the matched variable with explicit bindings:

```rs
Option.map = λoption λf
  match option {
    (Some value): (Some (f value))
    (None): None
  }
```

Rules can also have patterns.
It functions like match expressions with explicit bindings:

```rs
(Option.map (Some value) f) = (Some (f value))
(Option.map None f) = None
```

But with the extra ability to match on multiple values at once:

```rs
data Boolean = True | False

(Option.is_both_some (Some lft_val) (Some rgt_val)) = True
(Option.is_both_some lft rgt) = False
```

## Advanced

Similarly to dups and sups, lambdas and applications can have labels too.  
For example, data types can be encoded as tagged lambdas:
```rs
// data Bool = T | F
T = @#Bool t @#Bool f t
F = @#Bool t @#Bool f f

// data List = (Cons x xs) | Nil
Cons = @x @xs @#List c @#List n (#List c x xs)
Nil  =        @#List c @#List n n
```

When encoding the pattern matching, the application can then use the same label:

```rs
// not = @bool match bool { T: (F) F: (T) } 
not = @bool (#Bool bool F T)
```

This allows, in some limited* scenarios, automatic vectorization:
```rs
// vectorizes to: (Cons F (Cons T (Cons F Nil)))
main = (not (Cons T (Cons F (Cons T Nil))))
```

The tagged lambda and applications are compiled to `dup` `inet` nodes with different tag values for each label. This allows for different commutation rules, check [HVM-Core](https://github.com/HigherOrderCO/hvm-core/tree/main#language) to learn more about it.

### *limitations:
- When using a built-in ADT (created using the `data` keyword), it must not contain fields 
- The function must not be recursive
- There must not be labels in common between the function and what you want to vectorize over

## Compilation of Terms to HVM-core

How terms are compiled into interaction net nodes?

HVM-Core has a bunch of useful nodes to write IC programs.
Every node contains one `main` port `0` and two `auxiliary` ports, `1` and `2`.

There are 6 kinds of nodes, Erased, Constructor, Reference, Number, Operation and Match.

A lambda `λx x` compiles into a Constructor node.
An application `((λx x) (λx x))` also compiles into a Constructor node.
We differentiate then by using the ports.

```
  0 - Points to the lambda occurrence      0 - Points to the function
  |                                        |
  λ                                        @
 / \                                      / \
1   2 - Points to the lambda body        1   2 - Points to the application occurrence
|                                        |
Points to the lambda variable            Points to the argument
```

So, if we visit a Constructor at port 0 it's a Lambda, if we visit at port 2 it's an Application.

Also, nodes have labels, we use the label to store data in the node's memory and also differentiate them.

The `Number` node uses the label to store it's number.
An `Op2` node uses the label to store it's operation.
And a `Constructor` node can have a label too! The label is used for `Dup` and `Tuple` nodes.

Check [HVM-Core](https://github.com/HigherOrderCO/hvm-core/tree/main#language) to know more about.

### Runtime and Compiler optimizations

The HVM-Core is an eager runtime, for both CPU and parallel GPU implementations.
Because of that, is recommended to use [supercombinator](https://en.wikipedia.org/wiki/Supercombinator) formulation to make terms be unrolled lazily, preventing infinite expansion in recursive function bodies.

Consider the code below:
```rs
Zero = λf λx x
Succ = λn λf λx (f n)
ToMachine = λn (n λp (+ 1 (ToMachine p)) 0)
```
The lambda terms without free variables are extracted to new definitions.
```rs
ToMachine0 = λp (+ 1 (ToMachine p))
ToMachine = λn (n ToMachine0 0)
```
Definitions are lazy in the runtime. Lifting lambda terms to new definitions will prevent infinite expansion.

Consider this code:
```rs
Ch_2 = λf λx (f (f x))
```
As you can see the variable `f` is used more than once, so HVM-Lang optimizes this and generates a duplication tree.
```rs
Ch_2 = λf λx dup f0 f0_ = f; dup f1 f1_ = f0_ = (f0 (f1 x))
```
