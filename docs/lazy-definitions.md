# Making recursive definitions lazy

The HVM-Core is an eager runtime, for both CPU and parallel GPU implementations. Recursive terms will unroll indefinitely.

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

This optimization is done automatically by hvm-lang.