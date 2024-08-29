# Program to calculate fibonacci numbers.

# Calculates fibonacci numbers recursively.
# Although branching recursion is usually a good idea to parallelize,
# it makes this code run in exponential time.
def fib_recursive(n: u24) -> u24:
  switch n:
    case 0:
      return 0
    case 1:
      return 1
    case _:
      return fib_recursive(n-2) + fib_recursive(n-2 + 1)

# Calculates fibonacci numbers iteratively (tail-recursively).
# This function is inherently sequential, but runs in linear time.
def fib_iterative(n: u24) -> u24:
  bend a=0, b=1, n:
    when n != 0:
      return fork(b, a + b, n - 1)
    else:
      return a

def main() -> u24:
  # With the iterative version, we can calculate large fibonacci numbers
  # While with the recursive version, we will quickly run out of memory.
  # Note that for numbers larger than 36 the result will overflow the space of the 24-bit integer.
  # But we can run any number we want reasonably fast.
  return fib_iterative(30)

  # With the recursive version we create a tree with exponential size.
  # For numbers larger than ~45, this will hit the maximum HVM memory and crash.
  # Try uncommenting and running this line and compare the execution time.
  #return fib_recursive(20)