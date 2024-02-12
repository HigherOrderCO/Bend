# HVM-Lang

HVM-Lang is a lambda-calculus based language and serves as an Intermediate Representation for HVM-Core, offering a higher level syntax for writing programs based on the [Interaction-Calculus](https://github.com/VictorTaelin/Interaction-Calculus#interaction-calculus).

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

## Usage

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

There are compiler options through the CLI. [Click here](docs/compiler-options.md) to learn about them.

## Syntax

HVM-Lang files consists of a series of definitions, which bind a name to a term. Terms can be lambdas, applications, or other terms.

Here's a lambda where the body is the variable `x`:
```rs
id = λx x
```

Lambdas can also be defined using `@`.
To discard the variable and not bind it to any name, use `*`:
```hs
True  = @t @* t
False = λ* λf f
```

Applications are enclosed by `(` `)`.
```rs
(λx x λx x λx x)
```
This term is the same as:
```rs
(((λx x) (λx x)) (λx x))
```
Parentheses around lambdas are optional. Lambdas have a high precedence

```rust
(λx a b) == ((λx a) b) != (λx (a b))
```

`*` can also be used to define an eraser term.
It compiles to an `inet` node with only one port that deletes anything that’s plugged into it.
```rs
era = *
```

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

Strings are delimited by `"` `"` and support Unicode characters.
```rs
main = "Hello, 🌎"
```
A string is desugared to a String data type containing two constructors, `String.cons` and `String.nil`.
```rs
// These two are equivalent
StrEx1 = "Hello"

data String = (String.cons head tail) | String.nil
StrEx2 = (String.cons 'H' (String.cons 'e', (String.cons 'l' (String.cons 'l', (String.cons 'o' String.nil)))))
```

Characters are delimited by `'` `'` and support Unicode escape sequences. They have a numeric value associated with them.
```
main = '\u4242'
```

Lists are delimited by `[` `]` and elements can be optionally separated by `,`.
```rs
ids = [3, 6, 9 12 16]
```
A list is desugared to a List data type containing two constructors, `List.cons` and `List.nil`.
```rs
// These two are equivalent
ListEx1 = [1, 2, 3]

data List = (List.cons head tail) | (List.nil)
ListEx2 = (List.cons 1 (List.cons 2 (List.cons 3 List.nil)))
```

### More features

Key:
- &#128215;: Basic resources
- &#128217;: Intermediate resources
- &#128213;: Advanced resources

Other features are described in the following documentation files:

- &#128215; Lazy definitions: [Making recursive definitions lazy](docs/lazy-definitions.md)
- &#128215; Data types: [Defining data types](docs/defining-data-types.md)
- &#128215; Native numbers and operations: [Native numbers](docs/native-numbers.md)
- &#128217; Duplications and superpositions: [Dups and sups](docs/dups-and-sups.md)
- &#128217; Scopeless lambdas: [Using scopeless lambdas](docs/using-scopeless-lambdas.md)
- &#128217; Tagged lambdas and applications: [Automatic vectorization with tagged lambdas](docs/automatic-vectorization-with-tagged-lambdas.md)
- &#128213;: Fusing functions: [Writing fusing functions](docs/writing-fusing-functions.md)

## Further reading

- &#128217; [Compilation and readback](docs/compilation-and-readback.md)
- &#128217; [Old HVM wiki learning material](https://github.com/HigherOrderCO/HVM/wiki/HVM-Wiki). It is outdated, but it can still teach you some of the basics.
