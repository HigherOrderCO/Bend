# HVM-Lang

HVM-Lang is an Intermediate Representation for HVM-Core that provides a higher level syntax to write programs based on the Interaction-Calculus.

## Installation

With the nightly version of rust installed, clone the repository:
```bash
git clone git@github.com:HigherOrderCO/hvm-lang.git

cd hvm-lang
```

Install with cargo:
```bash
cargo install --path .
```

## Hello World!

First things first, let's write a basic program that adds the numbers 3 and 2.

```hs
main = (+ 3 2)
```

HVM-Lang searches for the `main | Main` definitions as entrypoint of the program.

To run a program using hvm-lang you can use the argument `run`, like below:
```bash
hvm-lang run <file>
```

It will show the number 5 and also some stats like time and rewrites.

You can pass the option `--mem <size>` to limit the memory amount of the runtime. The default is 1GB.
```bash
hvm-lang --mem 65536 run <file>
```

To compile a program you can use the `compile` argument:
```bash
hvm-lang compile <file>
```
It will output to stdout the compiled file.

## Language Syntax

HVM-Lang syntax consists in Terms and Definitions.
A Term is something that holds value, it can be a Number, an Application, a Function, etc. A Definition points to a Term.

Here we have a definition 'two' to the number 2.
```rs
two = 2
```

Here we have a lambda, the lambda's body is the variable x.
```rs
id = λx x
```

Operations can handle just 2 terms at once.
```rs
some_val = (+ (+ 7 4) (* 2 3))
```
The current operations are: `+, -, *, /, %, ==, !=, <, >, &, |, ^, ~, <<, >>`.

A let term, we use it to bind some value to the next term, in this case `(* result 2)`.
```rs
let result = (+ 1 2); (* result 2)
```

It is possible to define tuples.
```rs
tup = (2, 2)
```

And pattern-match the tuple with the let.
```rs
let (x, y) = tup; (+ x y)
```

It is possible to duplicate terms using `dup`.
```rs
// the number 2 in church encoding using dup.
ch2 = λf λx dup f1 f2 = f; (f1 (f2 x))

// the number 3 in church encoding using dup.
ch3 = λf λx dup f0 f1 = f; dup f2 f3 = f0; (f1 (f2 (f3 x)))
```

HVM-Lang has a match syntax for machine numbers.
We match the case 0 and the case where the number is greater
than 0, p binds the value of the matching number - 1.
```rs
to_church = λn match n {
  0: λf λx x;
  1+p: λf λx (f (to_church p f x))
}
```

Also, it is possible to use global lambdas, where the variable occurs outside it's body:
```rs
($a (λ$a 1 λb b))
```
This term will reduce to:
```
(λb b 1)
1
```

## Terms to Nodes

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
```hs
Ch_2 = λf λx (f (f x))
```
As you can see the variable `f` is used more than once, so HVM-Lang optimizes this and generates a duplication tree.
```
Ch_2 = λf λx dup f0 f0_ = f; dup f1 f1_ = f0_ = (f0 (f1 x))
```

### Planned features

- Data types
- Pattern matching
