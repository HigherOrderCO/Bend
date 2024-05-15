Bend in X minutes - the ultimate guide!
=======================================

**Note: you shouldn't be here, you sneaky dev! This is a WIP. Get out!**

Bend is a high-level, massively parallel programming language. That means it
feels like Python, but scales like CUDA. It runs on CPUs and GPUs, and you don't
have to do anything to make it parallel - as long as your code isn't "helplessly
sequential", it **will** use 1000's of threads! In a single thread, it is still
not very fast. HVM2's compiler is very, very immature (even against HVM1, let
alone SOTA compilers) - but it will quickly improve over time.

It is important to keep in mind Bend isn't perfect, and we made an early release
in order to iteratively evolve the tech with the community (join our
[Discord](https://Discord.HigherOrderCO.com)!). So, if you understand that, and
is willing to be an early adopter of this tech - then, this guide will teach you
how to apply Bend in practice to build parallel programs in a whole new way!

If you'd like to have a more in-depth technical dive, check HVM2's
[paper](http://paper.HigherOrderCO.com/). If you'd like an entertaining,
intuitive explanation of how this is possible, check HVM1's classic
[HOW.md](https://github.com/HigherOrderCO/HVM/blob/master/guide/HOW.md). But if
you just want to dive straight into action - this guide is for you. Let's go!

Installation
------------

To use Bend, first, install [Rust](https://rust-lang.org/):

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Then, install Bend itself with:

```
cargo install bend
```

To test if it worked, type:

```
bend --help
```

For any help, reach us in our [Discord](https://discord.HigherOrderCO.com/)!

Hello, World!
-------------

As we said, Bend *feels* like Python - in some ways. It is high-level, you can
easily create objects and lists, there are ifs and loops. In some ways, it is
different: there is some Haskell in it, in the sense algebraic datatypes,
pattern-matching and recursion play an important role. You can think of it as
the midway through the goal of compiling modern languages to HVM. And Bend was
made from scratch to fully use HVM's potential. Here is the "Hello, world!":

```python
def main():
  return "Hello, world!"
```

Wait - there is something strange there. Why `return`, not `print`? Well, *for
now* (you'll read these words a lot), Bend doesn't have IO yet. We plan to
introduce it the upcoming weeks. So, *for now*, all you can do is perform
computations, and see a result. Run the program above as:

```
bend run main.bend
```

If all goes well, you should see "Hello, world!". The `bend run` command uses
the reference interpreter, which is slow, but reliable. In a few moments, we'll
teach you how to compile code to run on parallel CPUs and GPUs. For now, let's
focus in learning some fundamentals!

Basic Functions and Datatypes
-----------------------------

In Bend, functions are pure: they receive something, and they return something.
That's all. Here is a function that tells you how old you are:

```python
def am_i_old(age):
  if age < 18:
    return "you're a kid"
  else:
    return "you're an adult"

def main():
  return am_i_old(32)
```

That is simple enough, isn't it? Here is one that returns the distance between
two points:

```python
def distance(ax, ay, bx, by):
  dx = bx - ax
  dy = by - ay
  return sqrt(dx ** 2 + dy ** 2)


```

This isn't so pretty. Could we use tuples instead? Yes:

```python
def distance(a, b):
  (ax, ay) = a
  (bx, by) = b
  dx = b_x - a_x
  dy = b_y - a_y
  return sqrt(dx ** 2 + dy ** 2)
```

So far, this does look like Python, doesn't it? What about objects? Well - here,
we have a difference. In Python, you have classes. In Bend, we just have the
objects, without the "methods". This is how we create a 2D vector:

```python
object V2 { x, y }

def distance(a, b):
  open V2: a
  open V2: b
  dx = b.x - a.x
  dy = b.y - a.y
  return sqrt(dx ** 2 + dy ** 2)
```

This doesn't look too different, does it? What is that `open` thing, though? It
just tells Bend to *consume* the vector, `a`, "splitting" it into its
components, `a.x` and `a.y`.

Is that really necessary? Actually, no - not really. But, for now, it is. This
has to do with the fact Bend is an affine language, but... we'll not get too
deep into that. In the future, we'll get rid of that noise. For now, just
remember that accessing the fields of an object requires you to `open` it.

Other than objects, Bend has a last way to encode data: algebraic datatypes
(ADTs). If that sounds scary, don't worry: it just means "an object with a tag".
That's all there is to it. A classic example of this is a "shape", which can be
either a circle or a rectangle. Here's how you can define it in Bend:

```python
type Shape:
  Circle { radius }
  Rectangle { width, height }

def area(shape):
  match shape:
    case Shape/Circle:
      return 3.14 * shape.radius ** 2
    case Shape/Rectangle:
      return shape.width * shape.height
```

In this example, `Shape` is an ADT with two variants: `Circle` and `Rectangle`.
The `area` function uses pattern matching to handle each variant appropriately.
Note that, just like the `open` syntax "consumes" an object, giving us access
to its fields, the `match` syntax "consumes" a datatype, giving us access to the
correct fields, on each respective branch.

To test all these functions, we can just print them. Here's a full example:

```python
object V2 { x, y }

def distance(a, b):
  open V2: a
  open V2: b
  dx = b.x - a.x
  dy = b.y - a.y
  return sqrt(dx ** 2 + dy ** 2)

def main():
  with IO:
    a = V2 { x: 10.0, y: 10.0 }
    b = V2 { x: 20.0, y: 20.0 }
    d = distance(a, b)
    print("Distance is: " ++ V2/show(a, b))
```

Parallel Algorithms
-------------------

Now, let's get straight to the fun part: how do we implement parallel algorithms
with Bend? Well, actually not. Before we get there, we must talk. You might have
noticed we have avoided loops, and mutating variables, so far. That's because,
in these situations, Bend diverges from Python in some ways. For example, a
Python developer might try expect to be able to write:

```python
def parity(x):
  result = "odd"
  if x % 2 == 0:
    result = "even"
  return result
```

But this is actually not valid in Bend. First, because local variables are
immutable - we can't mutate them. Second, because ifs and matches are
expressions, not statements. A valid program would be, instead:

```python
def is_even(x):
  if x % 2 == 0:
    return "even"
  else:
    return "odd"
```

This may sound annoying - and it is. But Bend does provide a series of tools to
amend this problem, and, in many cases, we can emulate a "mutable" style that
makes it feel just like Python. For example, the program below is valid Bend:

```python
def foo():
  obj = V2 { x: 10.0, y: 10.0 }
  obj.x += 5.0
  obj.y -= 3.0
  return obj
```

We'll elaborate on this in a moment. For now, let's focus in another subject
we've not covered: loops. How do we loop in bend? If Bend is pure and all
variables are immutable, then, loops would be pretty useless; after all, the
point of a loop is to either change local variables or do effects. But that's
not the only problem of loops: the fact they're sequential (0, then 1, then 2,
then 3...) is really, really bad for parallelism.

So, to simplify and reinforce the design of algorithms that are actually
parallel-friendly, Bend has banned loops entirely. I know, that sounds scary,
but, don't worry: it does replace these with some things that are equivalently
powerful. It **is** a little bit less intuitive. But it is also extremely
general, and getting comfortable with it will be rewarding. These things are two
syntax: the **fold**, and the **bend**.

### Trees, Folds and Bends

In Python, the most flexible container is the list, which allows you to store a
bunch of elements in sequence. In Bend, the flagship structure is the tree:

```
type Tree:
  Node { lft, rgt }
  Leaf { val }
```

A Tree is an ADT with two variants: `Node` and `Leaf`. By combining layers of
nodes and leaves, we can store elements just like lists... except that, instead
of being a linear sequence, they form a hierarchical structure: For example, to
store the numbers 1, 2, 3, 4, we could use the following tree:

```python
nums = Node { lft: 1, rgt: Node { lft: 2, rgt: Node { lft: 3, rgt: 4 }}}
```

Eh - that's quite ugly, isn't it? Of course, there is a syntax for it:

```python
nums = ![1, ![2, ![3, 4]]]
```

That's prettier... but how is that better than a list? The difference is that,
using trees, we can actually balance it, in a way that decreases the *maximum
depth*. For example, the same set could be represented as:

```python
nums = ![![1,2], ![3,4]]
```

And, as it turns out, this branching-like structure makes a *huge* difference
for parallelism. Just like Python provides for-loops to create and consume
lists, Bend provides constructs to create and consume trees.

### Fold: consuming a tree

The principle of a fold is that it replaces every `![fst,snd]` of a tree by a
function `F(fst,snd)` of your choice, and every leave `val` of a tree by a
function `F(val)` of your choice too. For example, consider the function below:

```python
def sum(tree):
  fold tree:
    case Node:
      return tree.lft + tree.rgt
    case Leaf:
      return tree.val * 2
```

To visualize how it works, remember the following tree:

```python
nums = ![![1,2], ![3,4]]
```

Based on the fold's code, it should replace every `![lft,rgt]` on that tree by
`(lft+rgt)`, and we should replace every `val` by `val*2`. The result becomes:

```python
nums = ((1*2 + 2*2) + (3*2 + 4*4))
```

Notice how the tree of data has been transformed in a tree of operations. Since
these operations are independent, HVM will compute them in parallel, like this:

```python
nums = ((2 + 4) + (6 + 16))
nums = (8 + 22)
nums = 30
```

Notice how `(2 + 4)` and `(6 + 16)` were executed at the *same time*, in
parallel. If the tree wasn't balanced, that wouldn't be possible. For example:

```python
nums = (2 + (4 + (6 + 16)))
nums = (2 + (4 + 22))
nums = (2 + 26)
nums = 28
```

In this case, all operations happened one after the other, sequentially.

In Bend, `fold` is the primary mechanism to consume a structure, computing a
result from it in parallel. It doesn't work just for this specific tree: it
works for *any* ADT you create. And the result doesn't need to be a single
number: it can be anything. For example, you can use a fold to convert a
quadtree (format used to represent game maps) to a JSON (which is also a tree!),
and store it to persist the game state. If you try hard enough, everything can
be turned into a tree!

## Bend: creating a tree

The `bend` is the opposite of the `fold`: instead of consuming something, it
creates something. The way it works is, in some ways, similar to a while loop,
but, admittedly, a little bit more bureaucratic, in 3 ways:

1. You must explicit an initial state.

2. You're allowed to mutate ONE local variable: the thing you're building.

3. Instead of being sequential, it can *branch* arbitrarily.

Here is how you create a perfect binary tree with copies of the number `7`:

```python
def main():

  bend x = 0:
    when x < 3:
      tree = ![go(x + 1), go(x + 1)]
    else:
      tree = 7

  do IO:
    print(tree)
```

The program above should print:

```python
![![![![7,7], ![7,7]], ![![![7,7], ![7,7]]]]]
```

Now, let's be honest: this is the least familiar syntax so far. But it is not
too hard, we promise. So, how does it work? Two ways to explain. First, with a
full example.

When the `bend` begins, the `x` value will be initialized as `0`. Then, as long
as `x < 3`, bend will create a node (`#[_,_]`), and repeat this operation with
`x + 1`, on each side. When `x == 3`, we stop, and just place a `7` in that
position. Like this:

```python
tree = go(0)
tree = ![go(1), go(1)]
tree = ![![go(2),go(2)], ![go(2),go(2)]]
tree = ![![![go(3),go(3)], ![go(3),go(3)]], ![![go(3),go(3)], ![go(3),go(3)]]]
tree = ![![![7,7], ![7,7]], ![![7,7], ![7,7]]]
```

You can kind of think of this as if `x` was a living cell that was duplicating
itself, forming a tree of descendants. In the end, you have a complete tree!

The second way to explain is for the devs offended by this ultra simplified
example: yes, `bend` is just a syntax-sugar for creating a recursive function,
immediately calling it, and assigning the result to a variable. In other words,
the program above is equivalent to:

```python
def go(x):
  if x < 3:
    return ![go(x + 1), go(x + 1)]
  else:
    return 7

def main():
  tree = go(0)
  do IO:
    print(tree)
```

Since this is operation is so common on Bend, we have a syntax for it! By adding
more states, we can create arbitrary trees that way. In fact, we can even
emulate loops. For example, consider the following Python loop:

```python
sum = 0
idx = 0
while idx < 10:
  sum = idx + sum
  idx = idx + 1
```

It could be emulated with a `bend` in the following manner:

```python
bend idx = 0:
  when idx < 10:
    sum = idx + go(idx + 1)
  else:
    sum = 0
```

Of course, if you do it, Bend's devs will be very disappointed with you.

## Example: Parallel Tree Sum

```python
def gen(d, x):
  switch d:
    case 0:
      return x
    case _:
      return (gen(d-1, x * 2 + 1), gen(d-1, x * 2))

def sum(d, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return sum(d-1, t.a) + sum(d-1, t.b)

def main:
  return sum(20, gen(20, 0))
```

TODO: explain

TODO: use bend/fold syntaxes

Benchmarks:
- 15.01s / 178 MIPS (Apple M3 Max, 1 thread)
- 1.35s / 1970 MIPS (Apple M3 Max, 16 threads) - 11x speedup
- 0.23s /  11823 MIPS (NVIDIA RTX 4090, 16k threads) - 65x speedup

# Example: Parallel Bitonic Sort

```python
def swap(s, a, b):
  switch s:
    case 0:
      return (a,b)
    case _:
      return (b,a)

def warp(d, s, a, b):
  switch d:
    case 0:
      return swap(s + (a > b), a, b)
    case _:
      (a.a,a.b) = a
      (b.a,b.b) = b
      (A.a,A.b) = warp(d-1, s, a.a, b.a)
      (B.a,B.b) = warp(d-1, s, a.b, b.b)
      return ((A.a,B.a),(A.b,B.b))

def flow(d, s, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return down(d, s, warp(d-1, s, t.a, t.b))

def down(d,s,t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return (flow(d-1, s, t.a), flow(d-1, s, t.b))

def sort(d, s, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return flow(d, s, sort(d-1, 0, t.a), sort(d-1, 1, t.b))
```

TODO: explain
TODO: use fold/bend syntaxes
TODO: should run with N=20 (2x higher CUDA MIPS), but 1-thread OOM's
TODO: this requires editing the CUDA file to use 7x7 instead of 8x7
Benchmarks:
- 12.33s / 102 MIPS (Apple M3 Max, 1 thread)
- 0.96s / 1315 MIPS (Apple M3 Max, 16 threads) - 12x speedup
- 0.24s / 5334 MIPS (NVIDIA RTX 4090, 16k threads) - 51x speedup

## ...

TODO: cover IO and so many other aspects :')
