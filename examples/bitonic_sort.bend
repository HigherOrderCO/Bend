# Implements bitonic sort on a list of numbers encoded as a tree of pairs.
# https://en.wikipedia.org/wiki/Bitonic_sorter
# Because we can't know when a tree of pairs is a leaf or a node, we pass the depth everywhere.

# Generates a tree of pairs with depth 'd' with numbers from 2^d to 0 at the leaves
def gen(d):
  bend d, x=0:
    when d:
      return (fork(d - 1, x * 2 + 1), fork(d - 1, x * 2))
    else:
      return x

# Adds all the numbers in a tree of pairs of depth 'd'
def sum(d, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return sum(d-1, t.a) + sum(d-1, t.b)

# Conditionally swaps the values of a pair
def swap(s, a, b):
  if s:
    return (b,a)
  else:
    return (a,b)

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

# Bitonic sort
def sort(d, s, t):
  switch d:
    case 0:
      return t
    case _:
      (t.a, t.b) = t
      return flow(d, s, (sort(d-1, 0, t.a), sort(d-1, 1, t.b)))

def main:
  # Generate a reverse sorted tree of numbers, sort them and then add them up
  return sum(18, sort(18, 0, gen(18)))