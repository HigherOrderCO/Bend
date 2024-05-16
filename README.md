# Bend

Bend is a massively parallel, high-level programming language.

Unlike low-level alternatives like CUDA and Metal, Bend has the feeling and
features of expressive languages like Python and Haskell, including fast object
allocations, higher-order functions with full closure support, unrestricted
recursion, even continuations. Yet, it runs on massively parallel hardware like
GPUs, with near-linear speedup based on core count, and zero explicit parallel
annotations: no thread spawning, no locks, mutexes, atomics. Bend is powered by
the [HVM2](https://github.com/HigherOrderCO/hvm2) runtime.

## Using Bend

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
def sum(depth, x):
  switch depth:
    case 0:
      return x
    case _:
      fst = sum(depth-1, x*2+0) # adds the fst half
      snd = sum(depth-1, x*2+1) # adds the snd half
      return fst + snd
    
def main:
  return sum(30, 0)
```

This code adds all numbers from 0 to 2^30, but, instead of a loop, we use a
recursive divide-and-conquer approach. Since this approach is *inherently
parallel*, Bend will run it multi-threaded. Some benchmarks:

- CPU, Apple M3 Max, 1 thread: **3.5 minutes**

- CPU, Apple M3 Max, 16 threads: **10.26 seconds**

- GPU, NVIDIA RTX 4090, 32k threads: **1.88 seconds**

That's a **111x speedup** by doing nothing. No thread spawning, no explicit
management of locks, mutexes. We just asked bend to run our program on RTX, and
it did. Simple as that. Bend isn't limited to a specific paradigm, like tensors
or actors. Any concurrent system, from shaders to Erlang-like actor models can
be emulated on Bend. For example, to render images in real time, we could simply
allocate an immutable tree on each frame:

```python
# given a shader, returns a square image
def render(depth, shader):
  bend d = 0, i = 0:
    when d < depth:
      color = (go(d+1, i*2+0), go(d+1, i*2+1))
    else:
      width = depth / 2
      color = demo_shader(i % width, i / width)
  return color

# given a position, returns a color
# for this demo, it just busy loops
def demo_shader(x, y):
  bend i = 0:
    when i < 100000:
      color = go(i + 1)
    else:
      color = 0x000001
  return color

# renders a 256x256 image using demo_shader
def main:
  return render(16, demo_shader)
```

And it would actually work (!!!). Even highly involved recursive algorithms,
such as this [Bitonic Sort based on tree rotations](examples/bitonic_sort.bend),
parallelizes linearly on Bend. Long-distance communication is performed by
*global beta-reduction* (as per the [Interaction
Calculus](https://github.com/VictorTaelin/Interaction-Calculus)), and
synchronized correctly and efficiently by
[HVM2](https://github.com/HigherOrderCO/HVM)'s *atomic linker*.

- To jump straight into action, check Bend's [GUIDE.md](https://github.com/HigherOrderCO/bend/blob/main/GUIDE.md).

- For an extensive list of features, check [FEATURES.md](https://github.com/HigherOrderCO/bend/blob/main/FEATURES.md).

- To understand the tech behind Bend, check HVM2's [paper](https://github.com/HigherOrderCO/HVM/raw/main/PAPER.pdf).
