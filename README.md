# Bend

Bend is a massively parallel, high-level programming language.

Unlike low-level alternatives like CUDA and Metal, Bend has the feeling and
features of expressive languages like Python and Haskell, including fast object
allocations, higher-order functions with full closure support, unrestricted
recursion, even continuations. Yet, it runs on massively parallel hardware like
GPUs, with near-linear speedup based on core count, and zero explicit parallel
annotations: no thread spawning, no locks, mutexes, atomics. Bend is powered by
the [HVM2](https://github.com/HigherOrderCO/hvm) runtime.

A Quick Demo
------------

[Watch the demo as a video](https://x.com/VictorTaelin/status/1791213162525524076)

You don't use 10% of your brain, so why use 1% of your computer?

Bend is a parallel programming language
- that feels like Python,
- but scales like CUDA.

Need a sorting network? Just write a function.

```python
# Sorting Network = just rotate trees!
def sort(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x,y) = tree
      lft   = sort(d-1, 0, x)
      rgt   = sort(d-1, 0, y)
      return rots(d, s, lft, rgt)

# Rotates sub-trees (Blue/Green Box)
def rots(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x, y) = tree
      return down(d, s, warp(d-1, s, x, y)
```

Need a fragment shader? Just write a function.

```python
# Image Rendering = create a pixel tree!
def new_image(depth, shader):
  bend d = 0, i = 0:
    when d < depth:
      color = (go(d+1,i*2+0), go(d+1,i*2+1))
    else:
      width = 1 << (depth / 2)
      color = fragment(i % width, i / width)
  return color

# Shader: computes the color of a pixel
def fragment(x, y):
  color = 0xFF0000
  # ... any computation here ...
  return color

def main:
  return new_image(16, fragment)
```

Need a fast map reduce? Just write a function.

```python
# Map-Reduce = simple divide-and-conquer!
def map_red(d, i, map, red):
  switch d:
    case 0:
      return map(i)
    case _:
      lft = map_red(d-1, i*2+0, map, red)
      rgt = map_red(d-1, i*2+1, map, red)
      return red(lft, rgt)

def main:
  map = lambda idx: idx * 777
  red = lambda x y: x * y + 1
  return map_red(24, 0, map, red)
```

and Bend will parallelize it for you!

With 1 CPU core:

```
$ bend run reduce.bend -s
Result: 5005402
- ITRS: 738197476
- TIME: 12.47s
- MIPS: 59.22
```

Now, with 16 CPU cores:

```
bend run-c reduce.bend -s
Result: 5005402
- ITRS: 738197476
- TIME: 0.79s
- MIPS: 933.18
```

Finally, with 16384 GPU cores: (with an RTX 4090)

```
bend run-cu reduce.bend -s
Result: 5005402
- ITRS: 738197476
- LEAK: 44580863
- TIME: 0.16s
- MIPS: 4534.43
```

That's a 100x speedup by doing nothing, other than using all the cores you already paid for.

## Using Bend

> Currently not working on Windows, please use [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) as a workaround.

First, install [Rust nightly](https://www.oreilly.com/library/view/rust-programming-by/9781788390637/e07dc768-de29-482e-804b-0274b4bef418.xhtml). Then, install both HVM2 and Bend with:

```sh
cargo +nightly install hvm
cargo +nightly install bend-lang
```

Finally, write some Bend file, and run it with one of these commands:

```sh
bend run    <file.hvm> # uses the Rust interpreter (sequential)
bend run-c  <file.hvm> # uses the C interpreter (parallel)
bend run-cu <file.hvm> # uses the CUDA interpreter (massively parallel)
```

You can also compile `Bend` to standalone C/CUDA files with `gen-c` and
`gen-cu`, for maximum performance. But keep in mind our code gen is still on its
infancy, and is nowhere as mature as SOTA compilers like GCC and GHC.

## Parallel Programming in Bend

To write parallel programs in Bend, all you have to do is... **nothing**. Other
than not making it *inherently sequential*! For example, the expression:

```python
(((1 + 2) + 3) + 4)
```

Can **not** run in parallel, because `+4` depends on `+3` which
depends on `(1+2)`. But the following expression:

```python
((1 + 2) + (3 + 4))
```

Can run in parallel, because `(1+2)` and `(3+4)` are independent; and it *will*,
per Bend's fundamental pledge:

> Everything that **can** run in parallel, **will** run in parallel.

For a more complete example, consider:

```python
# Sorting Network = just rotate trees!
def sort(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x,y) = tree
      lft   = sort(d-1, 0, x)
      rgt   = sort(d-1, 1, y)
      return rots(d, s, lft, rgt)

# Rotates sub-trees (Blue/Green Box)
def rots(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x,y) = tree
      return down(d, s, warp(d-1, s, x, y))

(...)
```

This
[file](https://gist.github.com/VictorTaelin/face210ca4bc30d96b2d5980278d3921)
implements a [bitonic sorter](https://en.wikipedia.org/wiki/Bitonic_sorter) with
*immutable tree rotations*. It is not the kind of algorithm you'd expect to
run fast on GPUs. Yet, since it uses a divide-and-conquer approach, which is
*inherently parallel*, Bend will run it multi-threaded. Some benchmarks:

- CPU, Apple M3 Max, 1 thread: **12.15 seconds**

- CPU, Apple M3 Max, 16 threads: **0.96 seconds**

- GPU, NVIDIA RTX 4090, 16k threads: **0.21 seconds**

That's a **57x speedup** by doing nothing. No thread spawning, no explicit
management of locks, mutexes. We just asked Bend to run our program on RTX, and
it did. Simple as that.

Bend isn't limited to a specific paradigm, like tensors or matrices. Any
concurrent system, from shaders to Erlang-like actor models can be emulated on
Bend. For example, to render images in real time, we could simply allocate an
immutable tree on each frame:

```python
# given a shader, returns a square image
def render(depth, shader):
  bend d = 0, i = 0:
    when d < depth:
      color = (fork(d+1, i*2+0), fork(d+1, i*2+1))
    else:
      width = depth / 2
      color = shader(i % width, i / width)
  return color

# given a position, returns a color
# for this demo, it just busy loops
def demo_shader(x, y):
  bend i = 0:
    when i < 5000:
      color = fork(i + 1)
    else:
      color = 0x000001
  return color

# renders a 256x256 image using demo_shader
def main:
  return render(16, demo_shader)
```

And it would actually work. Even involved algorithms parallelize well on Bend.
Long-distance communication is performed by *global beta-reduction* (as per the
[Interaction Calculus](https://github.com/VictorTaelin/Interaction-Calculus)),
and synchronized correctly and efficiently by
[HVM2](https://github.com/HigherOrderCO/HVM)'s *atomic linker*.

- To jump straight into action, check Bend's [GUIDE.md](https://github.com/HigherOrderCO/bend/blob/main/GUIDE.md).

- For an extensive list of features, check [FEATURES.md](https://github.com/HigherOrderCO/bend/blob/main/FEATURES.md).

- To understand the tech behind Bend, check HVM2's [paper](https://paper.higherorderco.com).

- Bend is developed by [HigherOrderCO.com](https://HigherOrderCO.com) - join our [Discord](https://discord.HigherOrderCO.com)!
