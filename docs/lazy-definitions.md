# Making recursive definitions lazy

In strict-mode, some types of recursive terms will unroll indefinitely.

This is a simple piece of code that works on many other functional programming languages and hangs on strict-mode:

```rust
Cons = λx λxs λcons λnil (cons x xs)
Nil  =        λcons λnil nil

Map = λf λlist
  let cons = λx λxs (Cons (f x) (Map f xs))
  let nil = Nil
  (list cons nil)

Main = (Map λx (+ x 1) (Cons 1 Nil))
```

The recursive `Map`  creates an infinite reduction sequence because each recursive call expands into another call to Map, never reaching a base case. Which means that functionally, it will reduce it infinitely, never reaching a normal form. 

For similar reasons, if we try using Y combinator it also won't work.

```rust
Y = λf (λx (f (x x)) λx (f (x x)))

Map = (Y λrec λf λlist
  let cons = λx λxs (Cons (f x) (rec f xs))
  let nil = Nil
  (list cons nil f))
```

By linearizing `f`, the `Map` function only expands after applying the argument `f`, because the `cons` function will be lifted to a separate top-level function by the compiler (when this option is enabled).

```rust
Map = λf λlist
  let cons = λx λxs λf (Cons (f x) (Map f xs))
  let nil = λf Nil
  (list cons nil f)
```

This code will work as expected, since `cons` and `nil` are lambdas without free variables, they will be automatically floated to new definitions if the [float-combinators](compiler-options.md#float-combinators) option is active, allowing them to be unrolled lazily by hvm.

The recursive part of the function should be part of a combinator that is not in an active position. That way it can be lifted into a top-level function which is compiled into a lazy reference thus preventing the infinite expansion. [Supercombinators](https://en.wikipedia.org/wiki/Supercombinator) can be used in order to ensure said lazy unrolling of recursive terms. Other combinator patterns can work as well, as long as they're lifted to the top level.

If you have a set of mutually recursive functions, you only need to make one of the steps lazy. This might be useful when doing micro-optimizations, since it's possible to avoid part of the small performance cost of linearizing lambdas.

### Automatic optimization

Bend carries out [match linearization](compiler-options.md#linearize-matches) and [combinator floating](compiler-options.md#float-combinators) optimizations, enabled through the CLI, which are active by default in strict mode.

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
