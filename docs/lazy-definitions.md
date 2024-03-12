# Making recursive definitions lazy

The HVM-Core is an eager runtime, for both CPU and parallel GPU implementations. Terms that use recursive terms will unroll indefinitely.

This means that for example, the following code will hang, despite being technically correct and working on Haskell:

```rs
data Nat = Z | (S p)

Y = λf (λx (f (x x)) λx (f (x x)))

Nat.add = (Y λaddλaλb match a {
	Z: b
	(S p): (S (add p b))
})

main = (Nat.add (S (S (S Z))) (S Z))
```

Because of that, is recommended to use [supercombinator](https://en.wikipedia.org/wiki/Supercombinator) formulation to make terms be unrolled lazily, preventing infinite expansion in recursive function bodies.

The `Nat.add` definition below can be a supercombinator if linearized.

```rs
Nat.add = λaλb match a {
	Z: b
	(S p): (S (Nat.add p b))
}
```

```rs
// Linearized Nat.add
Nat.add = λa match a {
	Z: λb b
	(S p): λb (S (Nat.add p b))
}
```

This code will work as expected, because `Nat.add` is unrolled lazily only when it is used as an argument to a lambda.

### Automatic optimization

HVM-lang carries out [match linearization](compiler-options#linearize-matches) and [combinator floating](compiler-options#float-combinators) optimizations, enabled through the CLI, which are active by default in strict mode.

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
