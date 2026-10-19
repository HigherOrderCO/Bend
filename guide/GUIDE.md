# Bend

Bend is a new programming language that combines Lean-like formal proofs with
C-like speeds and CUDA-like parallelism. It gives humans an ambiguity-free
language to communicate their intents to AIs, a compiler capable of mechanically
checking that the AI implemented these intents to unquestionable mathematical
correctness, and a compiler that runs that code fast on CPUs and GPUs.

## Hello, World!

Bend's syntax is Python-shaped, but its semantics are closer to Haskell / Lean,
while being resource-aware like Rust (if less annoyingly). A hello world is just
a typed definition returning an IO block:

```python
import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print("Hello, world!")
```

To run it, install Bend (`curl -fsSL https://bend-lang.com/install.sh | sh`)
and type `bend hello.bend`.

Bend is a *pure language*, with effects denoted via a Haskell-inspired
[IO Monad](https://wiki.haskell.org/Introduction_to_IO). It comes with a list
of built-in effects for files, networking, audio, graphics, input, and more,
and the user can extend them with foreign C and JS imports; more on that later.

## Core Features

### Types and Functions

A typical Bend program is a set of datatypes and functions over them:

```python
import Base

type Shape is Data:
  Circle{r: U32}
  Square{s: U32}

def area(x: Shape) -> U32:
  match x:
    case Circle{+r}:
      (3 * r * r : U32)
    case Square{+s}:
      (s * s : U32)

def main() -> U32:
  area(Square{5})
```

Bend, by default, is *affine*, meaning variables must be used, at most, once.

Here, `is Data` declares that `Shape` may be copied, while `is Type` keeps it
non-copiable. The `+` annotation before a variable name allows using it more
than once, if the variable is Data-kinded.

Bend does almost no inference, meaning it requires more annotations than similar
languages. This is what allows Bend's checker to be significantly faster than
other provers, and its error messages more precise, at the expense of programs
and proofs being more verbose. When the checker can't decide the type of an
expression, just annotate it, as in `{3 : U32}`.

### Closures

Functions are values and can be stored, passed, and returned from other functions.

```python
import Base

def adder(k: U32) -> U32 -> U32:
  x => (x + k : U32)

def main() -> U32:
  add2 = adder(2)
  add5 = adder(5)
  add5(add2(1))
```

A closure is affine: it can be called at most once, even when everything it
captures is `Data`. Only top-level definitions can be called freely. Partial
applications like `U32.add(2)` are closures too.

### Recursion and Termination

Bend uses recursion to repeat work. Tail calls compile to loops.

```python
import Base

def sum(xs: List<U32>, acc: U32) -> U32:
  match xs:
    case Nil{}:
      acc
    case Con{h, t}:
      sum(t, (acc + h : U32))

def main() -> U32:
  sum([1, 2, 3, 4], 0)
```

Here, `t` has one fewer element than `xs`, so `sum` eventually reaches the empty
list. Bend verifies termination by requiring recursive calls to use smaller
parts of their inputs, obtained through pattern matching. The check reads the
arguments of a recursive call from left to right: each must be passed unchanged
until one is a smaller part of its parameter, and the ones after it are free.
So, put the parameter that shrinks first. Also, is no `if` syntax yet. Use
`match` on `True{}` and `False{}` instead.

Termination is mandatory and mutual recursion is not allowed. Both restrictions
keep Bend's proofs sound, as a function that never returns could otherwise prove
anything. A loop bounded by the outside world, like a server's, counts down a
`Nat` fuel argument instead, and two mutually recursive functions become one def
with an extra argument selecting which to run. A `def` marked `@unsafe` recurses
freely and may call a def written below it, but falls outside Bend's proof
guarantees: `bend` runs it, but a check prints SOME PROOFS FAIL and names every
def that relies on it. Types are not code, so the order binds only defs: two
datatypes, or a datatype and a type-level def, may name each other in any order.

A `match` inspects a parameter or a variable bound by a pattern, never a
computed value: `match sum(xs, 0):` is rejected. Scrutinees follow binder order,
and a `let` may not precede a `match` on a parameter. To match on a computed
value, pass it to a helper that matches on its parameter.

### Parallelism

Bend's parallelism primitive is the parallel call notation:

```python
import Base

def pow2(+n: Nat) -> U32:
  match n:
    case 0n:
      1
    case 1n+p:
      a b = pow2(p) pow2(p) # parallel call
      (a + b : U32)

def main() -> IO(Unit):
  IO.print(U32.show(pow2!(20n))) # `!` runs on GPU
```

A parallel call promises the compiler two things:

1. The calls are independent.

2. They run in roughly the same time.

Since Bend is pure and affine, the first point always holds. The second is yours
to keep: if one call finishes before the other, the speedup will be sub-ideal.
Bend's current scheduler is a contention-free, binary fork-join machine: every
task is handed to a core exactly once and never moved afterwards. That makes it
fast and GPU-friendly, but you must keep the workload balanced.

A `!` after a function name marks a parallel call: `pow2!(20n)` hands that call,
and every parallel call inside it, to the GPU. When compiled to a native
executable, `pow2(20n)` runs in parallel on the CPU, while `pow2!(20n)` runs on
the GPU. The heap is fully unified, so, if your chip
has unified memory (as in Apple M-series processors), moving data from the CPU
to the GPU is a zero-cost operation. The GPU shines on uniform numeric work like
mandelbrot or nbody; divergent work like n-queens stays faster on the CPU. A
machine without a GPU runs `!` on the CPU (still in parallel). What the lanes
share also sets the speed: a `+` value read by every lane costs an atomic per
read. Read `bend guide shaders` before you write a parallel app.

The JavaScript target ignores all that and just runs sequentially.

### Arrays

Arrays give Bend in-place mutation without giving up purity:

```python
import Base

def main() -> Array<U32> & U32:
  a = [0 : U32*8n] # new array with 8 copies of 0
  a[5] <- 42       # performs an in-place rewrite
  a[5]             # reads index 5
```

An `Array<T>` is a `Type`, so it has exactly one owner at all times. That is
what lets `a[5] <- 42` overwrite the slot and hand back the same array, with no
copy. A read hands the array back next to the element for the same reason: if
it returned only the element, the array would be gone. Indexes wrap around. A
write followed by another statement re-binds its array: `a[5] <- 42` on its own
line is `a = a[5] <- 42`. As the last statement it is the written array. The
slot count after `*` is a power of two; `[0 : U32^3n]` names the depth instead.

The `a[i]` sugar assumes `Array<U32>`. For other element types, call
`Array.get` (`Data` elements; else `Array.swap`) and `Array.set` directly, and
`Array.clone` when you need two copies. Read Bend's Base for reference. This
will be generalized soon!

### Quantities

A quantity says how many times a variable may be used.

```python
import Base

# -A: erased (gone at runtime)
#  n: affine (used at most once)
# +x: reusable (requires A to be Data)
def replicate(-A: Data, n: Nat, +x: A) -> List<A>:
  match n:
    case 0n:
      Nil{}
    case 1n+p:
      x <> replicate(A, p, x)

def main() -> List<U32>:
  replicate(U32, 3n, 7)
```

Erased variables can only appear in types and proofs: the checker sees them, the
compiler deletes them. Affine variables are the default, and dropping one is
always free. Reusable variables require `Data`: functions, arrays and IO handles
are `Type`, so they can never be copied. Note that `main` marks nothing: you
write `+` where you need the copies, and `replicate` pays for them with a
reference count at runtime. Matching a `+` value hands out `+` fields; on a
plain one, write `+r` in the pattern to make a field reusable.

### Kinds

Every type has a kind, which caps how many times its values may be used.

```python
import Base

#  a: a quantity (&0, &1 or &2)
# -A: a type whose values may be used a times
def length(a, -A: Kind(a), xs: List<a, A>) -> Nat:
  match xs:
    case Nil{}:
      0n
    case Con{h, t}:
      1n+length(a, A, t)

def main() -> Nat:
  length(&2, U32, [1, 2, 3])
```

`Type` is short for `Kind(&1)` and `Data` for `Kind(&2)`, so a `Kind(a)`
parameter accepts both: `length(&1, U32 -> U32, fs)` counts a list of closures
just as well. A bare `a` in a parameter list is short for `-a: Quant`. Base
declares `type List<a, -A: Kind(a)> is Kind(a)`, making a list exactly as
reusable as its elements: `List<U32>` is short for `List<&1, U32>`, and
`+List<U32>` for `List<&2, U32>`. The short form needs the type declared above
it: a type named before its declaration spells every parameter, quantities
included. A type holding two element types combines their quantities with
`a <&> b`, the smaller of the two.

### Templates

A template receives its argument as syntax and inlines it at compile time.

```python
import Base

# ~f: substituted at compile time, not passed at runtime
def twice(~f: U32 -> U32, x: U32) -> U32:
  f(f(x))

def main() -> U32:
  twice(~(x => (x + 1 : U32)), 40)
```

Template parameters come first in the parameter list, and a `~` argument must
be closed: it may mention top-level defs, but no local variable of the caller.
Each distinct set of `~` arguments compiles to its own copy of `twice`, so `f`
costs nothing at runtime and, unlike a closure, may be called as many times as
you like. A template may call only templates declared above it. This is how
`List.map` is written in Base.

### Laws and Proofs

A law states a fact that must hold. It must be proven inside a paired def.

```python
import Base

# LAW: "for every x, x plus 0 equals x"
law add_zero:
  for x: Nat
  {Nat.add(x, 0n) == x : Nat}

# PROOF: case analysis:
# - base case: reflexivity
# - step case: induction, rewrite, reflexivity
def add_zero(x):
  match x:
    case 0n:
      {==}
    case 1n+p:
      %add_zero(p) : {1n+Nat.add(p, 0n) == 1n+_ : Nat}
      {==}

def main() -> {Nat.add(2n, 0n) == 2n : Nat}:
  add_zero(2n)
```

Laws are a critical feature in Bend, as they provide an ambiguity-free language
on which humans can state precise specs for AI's to implement. That is, instead
of writing a natural language prompt such as "implement a function that sorts a
list", users can write precise laws like "implement a function F such that, for
every list of numbers, `F(list)` returns the same numbers in ascending order".
Models are then guaranteed to respond with bug-free code, since Bend will demand
that they provide an actual proof.

> We envision that "law-driven development" will eventually become the way humans
> use AI to write and maintain large codebases, as it is the perfect middle point
> between having to code everything manually (laborious) and letting AI do it all
> via prompts without auditing a line of code (error/ambiguity-prone, unsecure).

By convention, a project keeps its laws in two files at its root. `LAWS.bend`
imports the code and states the laws, each an open claim: the human writes it,
the AI does not touch it. `PROOF.bend` imports `LAWS.bend` and proves each law
with a def of the same name (`law sorted` is proven by `def Laws.sorted`): the
AI writes it, along with the code. `bend PROOF.bend` is the gate: it prints
SOME PROOFS FAIL while any law is open or false, and ALL PROOFS CHECK once every
law holds.
bend refuses a `PROOF.bend` that sits beside a `LAWS.bend` without importing it.

`bend PROOF.bend --verdict` checks the proofs a second time, with a small kernel
that has a proof in Lean: it prints ALL PROOFS CHECK only when every def outside
Base is a valid proof, which bend2 and the kernel both accept, and which relies
on no `@unsafe` or foreign code. `-o PROOF.bendtt` writes the translation the
kernel reads; the translation has no proof, so read it to confirm a law.

Bend has no tactics: a proposition is a type, and a proof is a def of that type.
`{a == b : T}` is an equality; `{==}` proves it when both sides compute to the
same term. Matching refines the goal in each case, a recursive call is the
induction hypothesis, and `%e : P` rewrites with `e : {a == b : T}`: `P` is the
goal with `_` marking `b`, and the goal becomes `P` with `a` there. `exs y: T`
in a law asks for a witness, returned as `(y, proof)`. A failed step prints the
expected and observed terms; `?name` prints the goal, `?TODO` leaves it open,
and a law with no def is an open claim.

Since types are terms, a def may return a `Type`, like `def IsEven(n: Nat) ->
Type:`, which is all dependent types are. A `match e:` with no cases closes a
branch where `e : Empty`. To refute a clash like `e : {1n == 0n : Nat}`, rewrite
it through a motive `disc(_)`, where `disc` sends `0n` to `Empty` and `1n+p` to
`Unit`, and answer `Unit{}`. `{a != b : T}` is `{a == b : T} -> Empty`, and
`Equal.sym`, `Equal.trans` and `Equal.cong` are in Base.

### IO and Concurrency

Effects live in the `IO` type and are sequenced with `do` blocks:

```python
import Base

def greet(name: String) -> IO(String):
  do IO<String>:
    IO.sleep(1000)             # a step
    return "Hello, " ++ name   # return: wraps a pure value

def main() -> IO(Unit):
  do IO<Unit>:
    name : String <- IO.try(String, IO.get_env("USER")) # <-: binds a result
    chan : Chan(String) <- IO.fork(String, greet(name)) # runs concurrently
    IO.print("Waiting...")
    text : String <- IO.join(String, chan)
    IO.print(text)
```

Every bind is annotated, and `x : T = v` binds a pure value in the middle of a
block. A fallible effect answers `Result<&1, &1, U32 & String, A>`: `IO.try`
unwraps it or exits with the error, and `IO.die` exits with your own. `IO.args`
answers the command line, less the runtime's own options (a `--` ends them):
its head is the program as invoked, like C's `argv[0]`. A handle (`File`,
`Socket`, `Window`) is an affine, opaque value, so every effect on one hands it
back beside its result, and no program can forge or reuse one.

`TCP.listen(host, port)` and `UDP.bind(host, port)` bind the IPv4 literal
`host`: `"127.0.0.1"` serves this machine only, `"0.0.0.0"` every interface.
A bad address or a port above 65535 fails with `EINVAL`.

`Process.run(program, args, input, max_output, timeout_ms)` starts an
executable directly, with literal arguments rather than a shell. It inherits
the current directory and environment, writes UTF-8 `input` to its stdin, and
answers `Done{(status, (stdout, stderr))}` even when the exit status is not
zero. Both limits must be positive; failure to start, a timeout, or stdout
and stderr exceeding `max_output` bytes together answer `Fail`. The call
waits for the direct child and drains ready output; a descendant holding an
inherited pipe open does not extend the wait. Timeouts do not kill descendants.
A native build runs the child on an IO helper thread; the JavaScript lane
blocks while the child runs. On Linux, native builds need glibc 2.34 or newer
to close inherited descriptors. It does not sandbox the child: callers must
whether a command is trusted before executing it.

A Bend program is a set of computations interleaved by one event loop, as in
Node.js: each runs its pure code (in parallel, on every core) up to its next
effect, and one that waits on a socket, a sleep or a channel steps aside for the
others. `IO.fork` starts a computation and returns the channel its result will
arrive on; `IO.join` waits for it. Underneath are `IO.spawn`, `Chan.new`,
`Chan.send`, `Chan.recv` and `Chan.close`. `IO.within(A, ms, act)` races `act`
against a deadline and answers `None{}` if the deadline wins; the loser is not
cancelled. The program ends when every computation is done, or reports a
deadlock when the remaining ones all wait.

Every effect in Base is a def whose body is `import "./x.js"` plus a `.c` twin,
implemented by a host function named after the def, lowercased, dots to
underscores. You can add your own effects the same way. Only the event loop runs
them, so proofs, termination and the GPU never touch host code. In the other
direction, a JS file may `import Game from "./game.bend"` (with `bend2/main.ts`
preloaded), or from the `./game.mjs` that `-o game.mjs` writes, and call every
non-IO def, with constructors as `{$: "Name", field: value}` and `Nat` as
`BigInt`. A value crosses without a copy: an `Array`
argument is the caller's own array, updated in place, so copy it first if you
keep it.

### Monads

The `do` notation works for any monad, not just IO.

```python
import Base

def add_strs(a: String, b: String) -> Maybe<&2, U32>:
  do Maybe<&2, U32>:
    x : U32 <- U32.read(a) # a None here ends the block with None
    y : U32 <- U32.read(b)
    return (x + y : U32)

def main() -> Maybe<&2, U32>:
  add_strs("40", "2")
```

A `do M<xs.., R>:` block desugars each `x : A <- v` into `M.bind(xs.., A, R, v,
x => ..)` and each `return e` into `M.pure(xs.., R, e)`, so any type with those
two defs works: `IO`, `Maybe`, `Result`, or your own. The leading arguments
(here, the `&2` quantity) are passed along to both.

### Apps

Graphics in Bend are pure: a frame is an `Image`; `App` maps states to images.

```python
import Base

# tick: folds a frame's events into the next state; None quits the app
def tick(events: List<Event>, color: U32) -> IO(Maybe<U32>):
  match events:
    case Nil{}:
      IO.pure(Maybe<U32>, Some{color})
    case Con{Close{}, rest}:
      IO.pure(Maybe<U32>, None{})
    case Con{e, rest}:
      tick(rest, (color + 1 : U32))

def main() -> IO(Unit):
  # view: returns the state beside its image; Pix paints the whole frame
  App.run(~U32, ~App{+s => (s, Pix{s}), tick}, "Hello", 256, 256, 0)
```

An `Image` is a quadtree: `Pix{color}` paints a square, and `Qua{tl, tr, bl,
br}` splits it in four, so a frame is drawn by recursion like everything else,
in parallel if you want. Events are `Key`, `Mouse`, `Move`, `Look`, `Scroll`
and `Close`. `Scroll{x, y, dx, dy}` is a wheel or trackpad under the pointer;
its signed `F32` deltas scroll toward a page's top and left (a notch is 1 on
X11). For a first-person camera, `Window.grab(window, True{})` hides and holds
the cursor, and the mouse's motion comes as `Look{dx, dy}` (signed `F32`, in
`Move`'s units) until `Window.grab(window, False{})` or the window losing focus
lets it go.
`App.run` opens a window and calls `view` then `tick` once per frame, until
`tick` answers `None`. Since the state is affine, `view` must hand it back next
to the image. Underneath are `Window.open`, `Window.frame` and `Window.close`,
and `Audio.open`, `Audio.write` and `Audio.close` for sound. See
`demos/app_pong_game_2d` for a complete one.

### The Base Library

Base is small, and its names follow a scheme, so you can guess most of it:

```python
import Base

def main() -> String:
  +a = (6 * 7 : U32)         # sugar for U32.mul(6, 7)
  b  = U32.to_nat(a)         # conversions are T.to_x and T.from_x
  U32.show(a) ++ " = " ++ Nat.show(b)
```

Every def is named `Type.verb`, and the same verbs recur across `Nat`, `U32`
and `F32`: `add sub mul div mod` for arithmetic, `and or xor not shl shr` for
bits (`U32` only), `cmp` (returning `Cmp`, not on `F32`) and `is_eq is_ne is_lt
is_le is_gt is_ge` (returning `Bool`) for comparisons, `show` to `String` and
`read` back from it (answering a `Maybe`). Operators and `<`-style comparisons
are just sugar for these. Beyond numbers there are `Bool`, `Cmp`, `Maybe`,
`Result`, `List`, `Array`, a string-keyed `Map` (`new set get has del keys`;
`get` takes a default, and `get` and `has` hand the map back beside their
result), `Set` on top of it, the `Equal` lemmas, and the effects. `bend base`
prints all of it, `bend base --types` only the types, and `bend base Map` one
name and everything under it.

### Modules

A module is a file, and an import gives it a local name:

```python
# math.bend
import Base

def square(+x: U32) -> U32:
  (x * x : U32)
```

```python
# main.bend
import Base
import ./math.bend as M  # M.x now names every def of math.bend

def main() -> U32:
  M.square(7)
```

The alias is local to the importing file, and dots inside a name are just
characters: `U32.show` needs no module. A module's path is plain names
(letters, digits, `_` and `-`): `math.bend` is a module, `math.extra.bend` is
refused. A law left open in one file may be filled in another as
`def M.name(..)`, so a proof can ship separately from its claim.
`import 0x<hash>/main.bend as P` imports a package by content hash, fetched
from the hub and checked against it; `bend main.bend --publish` uploads a file
with everything it imports and prints that line.
`import <name>@<version>/main.bend as P` is the same package by the name
its author gave it on the hub, with `bend main.bend --publish
<name>@<version>` after `bend login`.

A publish is public and permanent, under BendHub's terms
(https://bend-lang.com/bender/terms#s18). Put a `LICENSE` file
next to your entry file, ideally opening with a line like
`SPDX-License-Identifier: MIT`; `--publish` takes every file named exactly
`LICENSE` beside a published file, and a package without one is MIT-0. You are
responsible for what you publish, so pick the license it may carry. Adding a
`LICENSE` changes a package's hash: publish it as a new version.

## Tooling

Bend is a single command:

```bash
bend file.bend            # check; run main (IO compiled; a value normalized)
bend file.bend -o file    # compile to a native binary (clang 14+; 19+ with `!`)
bend file.bend -o file.c  # emit the C source instead
bend file.bend -o file.js # emit the JS source instead
bend file.bend -o f.mjs   # emit an ES module of its non-IO defs, for JS to import
bend file.bend --verdict  # check; then recheck with the proven BendTT kernel
bend page.html -o dist    # bundle a web page that imports .bend files
./file --threads 8        # run a native binary on 8 CPU threads
./file --gpu off          # run ! calls on the CPU (the GPU is on by default)
./file --gpu 4GB          # cap the GPU's heap at 4GB
```

A `main` that returns `IO` runs compiled; one that returns a value is normalized
by the checker (slow for big work) and printed; a file with no `main` just
checks. A binary that uses `!` builds its GPU program too, as `file.gpu`, which
must stay beside it: on macOS it needs Metal, on Linux CUDA 12 at
`/usr/local/cuda`. On Linux a program with a Window needs `libx11-dev`, one
with Audio `libasound2-dev`. `bend guide` prints this text, `bend base` prints
the Base library (`bend base Map` prints one name and everything under it), and
`bend --help` lists the other commands.

## Syntax Reference

Every form of the language, grouped by where it appears. Operators, literals
and brackets are sugar for names in Base.

```python
# Top level
import Base                              # the prelude
import ./file.bend as M                  # a module; its defs are M.x
type D<a, -A: Kind(a)> is Kind(a):       # a datatype and its kind
  K{x: A, xs: List<a, A>}                # one constructor per line
def f(x: A, -y: B, +z: C) -> T:          # a def; the body follows
def f(x, y):                             # fills the law named f
def t(~g: A -> B, x: A) -> B:            # a template
law f:                                   # a claim, proven by def f
  for x: A                               # a parameter (also for -x, for +x)
  for y: B where P(y)                    # y is then the pair (y, P(y) proof)
  exs z: C                               # a witness the proof must return
  T                                      # the claim
@unsafe def f(x: A) -> T:                # skips the termination check
def f?(x: A) -> T:                       # the same, as a sugar
def e(x: A) -> IO(B):                    # a foreign effect
  import "./e.c"
  import "./e.js"
```

```python
# Types
Type  Data  Kind(q)                      # kinds; Type = Kind(&1), Data = Kind(&2)
Quant  &0  &1  &2  a <&> b               # quantities and their minimum
A -> B  @x:A -> B  @-x:A -> B            # functions: plain, dependent, erased
A & B  &x:A -> B  A | B                  # pairs, dependent pairs, sums
D<A>  +D<A>  D<&2, A>                    # a datatype; + makes it reusable
{a == b : T}  {a != b : T}               # equality and its negation
```

```python
# Terms
42  1.5  3n  'c'  "s"                    # U32, F32, Nat, Char, String
[a, b]  h <> t  (a, b)                   # a list, a cons, a tuple
K{a, b}  x => e  +x => e                 # a constructor, a lambda
f(a, b)  f!(a)  t(~g, a)                 # a call, on the GPU, of a template
(a + b * c : T)  {x : T}                 # operators over T; an annotation
[v : T*n]  [v : T^d]  a[i]  a[i] <- v    # an array of n or 2^d slots; a read, a write
{==}  %e : P; e2  %e@E : P; e2           # reflexivity, a rewrite, a named one
?name  ?TODO                             # print the goal; leave it open
```

```python
# Statements (a def, case, lambda or parenthesized body)
x = v  +x = v  -x = v                    # a let: affine, reusable, erased
(a, b) = v  K{x, y} = v                  # a destructuring let
a b = f(x) g(y)                          # a parallel let
match a b:                               # a match on one or more values
  case K{x, _} 1n+p:                     # patterns nest; _ catches the rest
do M<xs.., R>:                           # a monadic block over M.bind, M.pure
  x : A <- m                             # bind
  x : A = v                              # let
  m                                      # a Unit step
  return v                               # the result
```

Inside `(.. : T)`, `+ - * / %` call `T.add` through `T.mod`, `.&. .|. .^.` the
bit operations, `<< >>` the shifts (by a `Nat`), and `< <= > >=` the `T.is_lt`
family; without a `: T` they are refused. `&& ||` work on `Bool` and `++` on
`String` anywhere. Operators need spaces on both sides.
Equality of values is a call, `T.is_eq(a, b)`; `==` is only the type.
A `Nat` literal past `256n` is `U32.to_nat(n)` underneath, up to `4294967295n`.

## Under the Hood

Bend's compiler emits one C file, and that file is both the CPU program and the
GPU kernel. clang compiles it for the CPU. Metal (on Apple) or CUDA (on NVIDIA)
compiles the same file for the GPU. So a `!` call runs the same code on
whichever chip it lands on.

A term is one 64-bit word: small values are stored inline, everything else is a
pointer into a single heap shared by every core and by the GPU. There is no
garbage collector. Since values are affine, a `match` frees the node it opens
on the spot, and only `+` values carry a reference count. There is no C stack
either: each def compiles to a segment of a flat state machine, a call is a
jump, and a parallel call creates a join task plus one task per call, which the
scheduler deals across CPU or GPU lanes. `paper/BendRT.pdf` has the design and
the benchmarks.

Bend's theory has one universe and no positivity check: `Type : Type` holds,
and a datatype may recurse on the left of an arrow. What keeps this consistent
is a wall between two checking modes. Code that runs is checked *live*; types,
erased arguments and equations are checked *dead*. Dead code may loop forever or
inhabit `Empty`, but nothing dead ever counts as live evidence, and live
recursion must terminate. `bend2/bendtt.lean` is BendTT's kernel in Lean, with
a proof that no def it accepts has type `Empty` and that live code halts;
`--verdict` checks a file with it. `paper/BendTT.pdf` is the paper.

## Further Reading

- `demos/`: complete programs, including the game and its proof from the video.
- `bend2/base.bend`: the Base library, also printed by `bend base`.
- `paper/BendTT.pdf` and `paper/BendRT.pdf`: the type theory and the runtime.

## Extra

`bend guide shaders` prints "Shaders in Bend", a tutorial written by AIs for
AIs on how to write efficient shaders in Bend. It distills what building
`demos/app_slash_boss_3d` (120 FPS in pure Bend) taught. Read it before you
write a graphical or parallel app in Bend.

`bend guide effects` prints "Effects in Bend", an AI-written note (to be
revised by a human) on the C and JS side of custom effects. Read it before
you write one.
