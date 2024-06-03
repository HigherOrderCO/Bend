# Bend

Bend is a massively parallel, high-level programming language.

Unlike low-level alternatives like CUDA and Metal, Bend has the feeling and
features of expressive languages like Python and Haskell, including:

- fast object allocations
- higher-order functions with full closure support
- unrestricted recursion
- even continuations

Yet, it runs on massively parallel hardware like GPUs, with near-linear speedup
based on core count, and zero explicit parallel annotations: no thread spawning,
no locks, mutexes, atomics.

Bend is powered by the [HVM2] runtime.

[HVM2]: https://github.com/HigherOrderCO/hvm

## A Quick Demo

[![Bend live demo](https://github.com/VictorTaelin/media/blob/main/bend_live_demo.gif?raw=true)](https://x.com/i/status/1791213162525524076)

- For a more in-depth explanation on how to setup and use Bend, check
  [GUIDE.md](https://github.com/HigherOrderCO/bend/blob/main/GUIDE.md).

- For an extensive list of features, check
  [FEATURES.md](https://github.com/HigherOrderCO/bend/blob/main/FEATURES.md).

## Important Notes

- Bend is designed to excel in scaling performance with cores, supporting over
  10000 concurrent threads.
- The current version may have lower single-core performance.
- You can expect substantial improvements in performance as we advance our code
  generation and optimization techniques.
- Windows support is being worked on, for now you can use [WSL2].
- [We only support NVIDIA Gpus currently][HigherOrderCO/Bend/issues/341].

[WSL2]: https://learn.microsoft.com/en-us/windows/wsl/install
[HigherOrderCO/Bend/issues/341]: https://github.com/HigherOrderCO/Bend/issues/341

> If you're having issues or questions about Bend, please first read the [FAQ].

[FAQ]: https://github.com/HigherOrderCO/Bend/blob/main/FAQ.md 

## Installation

### Dependencies

```sh
# Install Rust if you haven't it already.
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# For the C runtime, install a C compiler. We recommend GCC 12+.
sudo apt install gcc

# On macOS
brew install gcc
```

For the CUDA runtime install [CUDA tookkit for Linux] version 12.x.

[CUDA tookkit for Linux]: https://developer.nvidia.com/cuda-downloads?target_os=Linux

### Install Bend

1. Install HVM2:

```sh
# HVM2 is HOC's massively parallel Interaction Combinator evaluator.
cargo install hvm

# Check if HVM is correctly installed and accessible.
hvm --version
```
2. Install Bend:

```sh
# This command will install Bend
cargo install bend-lang

# Check if Bend is correctly installed and accessible.
bend --version
```

### Getting Started

#### Running Bend Programs

```sh
bend run    <file.bend> # uses the Rust interpreter (sequential)
bend run-c  <file.bend> # uses the C interpreter    (parallel)
bend run-cu <file.bend> # uses the CUDA interpreter (massively parallel)
```

You can also compile Bend to standalone C/CUDA files with `gen-c` and
`gen-cu`, for maximum performance. But keep in mind our code gen is still in its
infancy, and is nowhere as mature as SOTA compilers like GCC and GHC.

You can use the -s flag to have more information on, reductions time the code
took to run Interaction per second (In millions).

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


- To understand the tech behind Bend, check HVM2's [paper](https://paper.higherorderco.com).

- Bend is developed by [HigherOrderCO](https://HigherOrderCO.com) - join our [Discord](https://discord.HigherOrderCO.com)!

## Note

It is very important to reinforce that, while Bend does what it was built to
(i.e., scale in performance with cores, up to 10000+ concurrent threads), its
single-core performance is still extremely sub-par. This is the first version of
the system, and we haven't put much effort into a proper compiler yet. You can
expect the raw performance to substantially improve on every release, as we work
towards a proper codegen (including a constellation of missing optimizations).
Meanwhile, you can use the interpreters today, to have a glimpse of what
massively parallel programming looks like, from the lens of a Pythonish,
high-level language!
