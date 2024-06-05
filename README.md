<h1 >Bend</h1>
<p>A high-level, massively parallel programming language</p>

## Index
1. [Introduction](#introduction)
2. [Important Notes](#important-notes)
3. [Install](#install)
4. [Getting Started](#getting-started)
5. [Speedup Example](#speedup-example)
6. [Additional Resources](#additional-resources)

## Introduction

Bend offers the feel and features of expressive languages like Python and Haskell. This includes fast object allocations, full support for higher-order functions with closures, unrestricted recursion, and even continuations.                             
Bend scales like CUDA, it runs on massively parallel hardware like GPUs, with nearly linear acceleration based on core count, and without explicit parallelism annotations: no thread creation, locks, mutexes, or atomics.                     
Bend is powered by the [HVM2](https://github.com/higherorderco/hvm) runtime.


## Important Notes

* Bend is designed to excel in scaling performance with cores, supporting over 10000 concurrent threads.
* The current version may have lower single-core performance.
* You can expect substantial improvements in performance as we advance our code generation and optimization techniques.
* We are still working to support Windows. Use [WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) as an alternative solution.
* [We only support NVIDIA Gpus currently](https://github.com/HigherOrderCO/Bend/issues/341).




## Install

### Install dependencies

#### On Linux
```sh
# Install Rust if you haven't it already.
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# For the C version of Bend, use GCC. We recommend a version up to 12.x.
sudo apt install gcc
```
For the CUDA runtime [install the CUDA toolkit for Linux](https://developer.nvidia.com/cuda-downloads?target_os=Linux) version 12.x.


#### On Mac
```sh
# Install Rust if you haven't it already.
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# For the C version of Bend, use GCC. We recommend a version up to 12.x.
brew install gcc
```


### Install Bend

1. Install HVM2 by running:
```sh
# HVM2 is HOC's massively parallel Interaction Combinator evaluator.
cargo install hvm

# This ensures HVM is correctly installed and accessible.
hvm --version
```
2. Install Bend by running:
```sh
# This command will install Bend
cargo install bend-lang

# This ensures Bend is correctly installed and accessible.
bend --version
```

### Getting Started
#### Running Bend Programs
```sh
bend run    <file.bend> # uses the Rust interpreter (sequential)
bend run-c  <file.bend> # uses the C interpreter (parallel)
bend run-cu <file.bend> # uses the CUDA interpreter (massively parallel)

# Notes
# You can also compile Bend to standalone C/CUDA files using gen-c and gen-cu for maximum performance.
# The code generator is still in its early stages and not as mature as compilers like GCC and GHC.
# You can use the -s flag to have more information on
  # Reductions
  # Time the code took to run
  # Interaction per second (In millions)
```

#### Testing Bend Programs
The example below sums all the numbers in the range from `start` to `target`. It can be written in two different methods: one that is inherently sequential (and thus cannot be parallelized), and another that is easily parallelizable. (We will be using the `-s`flag in most examples, for the sake of visibility)

#### Sequential version:
First, create a file named `sequential_sum.bend`
```sh
# Write this command on your terminal
touch sequential_sum.bend
```
Then with your text editor, open the file `sequential_sum.bend`, copy the code below and paste in the file.

```py
# Defines the function Sum with two parameters: start and target
def Sum(start, target):
    # If the value of start is the same as target, returns start
  if start == target:
    return start
    # If start is not equal to target, recursively call Sum with start incremented by 1, and add the result to start
  else:
    return start + Sum(start + 1, target)  

def main():
# This translates to (1 + (2 + (3 + (...... + (79999999 + 80000000)))))
  return Sum(1, 80000000)
```

##### Running the file
You can run it using Rust interpreter (Sequential)
```sh
bend run sequential_sum.bend -s
```

Or you can run it using C interpreter (Sequential)
```sh
bend run-c sequential_sum.bend -s
```

If you have a NVIDIA GPU, you can also run in CUDA (Sequential)
```sh
bend run-cu sequential_sum.bend -s
```

In this version, the next value to be calculated depends on the previous sum, meaning that it cannot proceed until the current computation is complete. Now, let's look at the easily parallelizable version.


#### Parallelizable version:
First close the old file and then proceed to your terminal to create `parallel_sum.bend`
```sh
# Write this command on your terminal
touch parallel_sum.bend
```
Then with your text editor, open the file `parallel_sum.bend`, copy the code below and paste in the file.

```py
# Defines the function Sum with two parameters: start and target
def Sum(start, target):
  # If the value of start is the same as target, returns start
  if start == target:
    return start
  # If start is not equal to target, calculate the midpoint (half), then recursively call Sum on both halves
  else:
    half = (start + target) / 2
    left = Sum(start, half)  # (Start -> Half)
    right = Sum(half + 1, target)
    return left + right

# Main function to demonstrate the parallelizable sum from 1 to 80000000
def main():
# This translates to ((1 + 2) + (3 + 4)+ ... (79999999 + 80000000)...)
  return Sum(1, 80000000)
```

In this example, the (3 + 4) sum does not depend on the (1 + 2), meaning that it can run in parallel because both computations can happen at the same time. 

##### Running the file
You can run it using Rust interpreter (Sequential)
```sh
bend run parallel_sum.bend -s
```

Or you can run it using C interpreter (Parallel)
```sh
bend run-c sequential_sum.bend -s
```

If you have a NVIDIA GPU, you can also run in CUDA (Massively parallel)
```sh
bend run-cu sequential_sum.bend -s
```

In Bend, it can be parallelized by just changing the run command. If your code **can** run in parallel it **will** run in parallel.


### Speedup Examples
The code snippet below implements a [bitonic sorter](https://en.wikipedia.org/wiki/Bitonic_sorter) with *immutable tree rotations*. It's not the type of algorithm you would expect to run fast on GPUs. However, since it uses a divide and conquer approach, which is inherently parallel, Bend will execute it on multiple threads, no thread creation, no explicit lock management.

#### Bitonic Sorter Benchmark

- `bend run`: CPU, Apple M3 Max: 12.15 seconds
- `bend run-c`: CPU, Apple M3 Max: 0.96 seconds
- `bend run-cu`: GPU, NVIDIA RTX 4090: 0.21 seconds

 <details>
  <summary><b>Click here for the Bitonic Sorter code</b></summary>
   
 ```py
 # Sorting Network = just rotate trees!
def sort(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x,y) = tree
      lft   = sort(d-1, 0, x)
      rgt   = sort(d-1, 1, y)
      return rots(d, s, (lft, rgt))

# Rotates sub-trees (Blue/Green Box)
def rots(d, s, tree):
  switch d:
    case 0:
      return tree
    case _:
      (x,y) = tree
      return down(d, s, warp(d-1, s, x, y))

# Swaps distant values (Red Box)
def warp(d, s, a, b):
  switch d:
    case 0:
      return swap(s ^ (a > b), a, b)
    case _:
      (a.a, a.b) = a
      (b.a, b.b) = b
      (A.a, A.b) = warp(d-1, s, a.a, b.a)
      (B.a, B.b) = warp(d-1, s, a.b, b.b)
      return ((A.a,B.a),(A.b,B.b))

# Propagates downwards
def down(d,s,t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return (rots(d-1, s, t.a), rots(d-1, s, t.b))

# Swaps a single pair
def swap(s, a, b):
  switch s:
    case 0:
      return (a,b)
    case _:
      return (b,a)

# Testing
# -------

# Generates a big tree
def gen(d, x):
  switch d:
    case 0:
      return x
    case _:
      return (gen(d-1, x * 2 + 1), gen(d-1, x * 2))

# Sums a big tree
def sum(d, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return sum(d-1, t.a) + sum(d-1, t.b)

# Sorts a big tree
def main:
  return sum(20, sort(20, 0, gen(20, 0)))
```

</details>
  
if you are interested in some other algorithms, you can check our [examples folder](https://github.com/HigherOrderCO/Bend/tree/main/examples)


### Additional Resources
 - To understand the technology behind Bend, check out the HVM2 [paper](https://docs.google.com/viewer?url=https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/PAPER.pdf).
 - We are working on an official documentation, meanwhile for a more in depth
     explanation check [GUIDE.md](https://github.com/HigherOrderCO/Bend/blob/main/GUIDE.md)
 - Read about our features at [FEATURES.md](https://github.com/HigherOrderCO/Bend/blob/main/FEATURES.md)
 - Bend is developed by [HigherOrderCO](https://higherorderco.com/) - join our [Discord](https://discord.higherorderco.com)!
