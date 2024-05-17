# Making recursive definitions lazy

In strict-mode, some types of recursive terms will unroll indefinitely.

This is a simple piece of code that works on many other functional programming languages, including hvm's lazy-mode, but hangs on strict-mode:

```rust
Cons = λx λxs λcons λnil (cons x xs)
Nil  =        λcons λnil nil

Map = λf λlist
  let cons = λx λxs (Cons (f x) (Map f xs))
  let nil = Nil
  (list cons nil)

Main = (Map λx (+ x 1) (Cons 1 Nil))
```

The recursive `Map` definition never gets reduced.
Using the debug mode `-d` we can see the steps:

```
(Map λa (+ a 1) (Cons 1 Nil))
---------------------------------------
(Map λa (+ a 1) λb λ* (b 1 Nil))
---------------------------------------
(Cons (λa (+ a 1) 1) (Map λa (+ a 1) Nil))
---------------------------------------
(Cons (λa (+ a 1) 1) (Nil λb λc (Cons (λa (+ a 1) b) (Map λa (+ a 1) c)) Nil))
---------------------------------------
...
```

For similar reasons, if we try using Y combinator it also won't work.

```rust
Y = λf (λx (f (x x)) λx (f (x x)))

Map = (Y λrec λf λlist
  let cons = λx λxs (Cons (f x) (rec f xs))
  let nil = Nil
  (list cons nil f))
```

By linearizing `f`, the `Map` function "fully reduces" first and then applies `f`.

```rust
Map = λf λlist
  let cons = λx λxs λf (Cons (f x) (Map f xs))
  let nil = λf Nil
  (list cons nil f)
```

This code will work as expected, since `cons` and `nil` are lambdas without free variables, they will be automatically floated to new definitions if the [float-combinators](compiler-options.md#float-combinators) option is active, allowing them to be unrolled lazily by hvm.

It's recommended to use a [supercombinator](https://en.wikipedia.org/wiki/Supercombinator) formulation to make terms be unrolled lazily, preventing infinite expansion in recursive function bodies.

If you have a set of mutually recursive functions, you only need to make one of the steps lazy. This might be useful when doing micro-optimizations, since it's possible to avoid part of the small performance cost of linearizing lambdas.

### Automatic optimization

HVM-lang carries out [match linearization](compiler-options.md#linearize-matches) and [combinator floating](compiler-options.md#float-combinators) optimizations, enabled through the CLI, which are active by default in strict mode.

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

Definitions are lazy in the runtime. Floating lambda terms into new definitions will prevent infinite expansion.

It's important to note that preventing infinite expansion through simple mutual recursion doesn't imply that a program lacks infinite expansion entirely or that it will terminate.
