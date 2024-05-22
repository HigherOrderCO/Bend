type Map_:
  Free
  Used
  Both {~a, ~b}

type Arr:
  Null
  Leaf {~a}
  Node {~a, ~b}

def swap(s, a, b):
  switch s:
    case 0:
      return Map_/Both{ a: a, b: b }
    case _:
      return Map_/Both{ a: b, b: a }
 

# Sort : Arr -> Arr
def sort(t):
  return to_arr(0, to_map(t))

# ToMap : Arr -> Map
def to_map(t):
  match t:
    case Arr/Null:
      return Map_/Free
    case Arr/Leaf:
      return radix(t.a)
    case Arr/Node:
      return merge(to_map(t.a), to_map(t.b))

# ToArr : U60 -> Map -> Arr
def to_arr(x, m):
  match m:
    case Map_/Free:
      return Arr/Null
    case Map_/Used:
      return Arr/Leaf{ a: x }
    case Map_/Both:
      return Arr/Node{ a: to_arr(x * 2, m.a), b: to_arr(x * 2 + 1, m.b) }

# Merge : Map -> Map -> Map
def merge(a, b):
  match a:
    case Map_/Free:
      return b
    case Map_/Used:
      return Map_/Used
    case Map_/Both:
      match b:
        case Map_/Free:
          return a
        case Map_/Used:
          return Map_/Used
        case Map_/Both:
          return Map_/Both{ a: merge(a.a, b.a), b: merge(a.b, b.b) }
  
# Radix : U60 -> Map
def radix(n):
  r = Map_/Used
  r = swap(n & 1, r, Map_/Free)
  r = swap(n & 2, r, Map_/Free)
  r = swap(n & 4, r, Map_/Free)
  r = swap(n & 8, r, Map_/Free)
  r = swap(n & 16, r, Map_/Free)
  r = swap(n & 32, r, Map_/Free)
  r = swap(n & 64, r, Map_/Free)
  r = swap(n & 128, r, Map_/Free)
  r = swap(n & 256, r, Map_/Free)
  r = swap(n & 512, r, Map_/Free)
  return radix2(n, r)

# At the moment, we need to manually break very large functiions into smaller ones
# if we want to run this program on the GPU.
# In a future version of Bend, we will be able to do this automatically.
def radix2(n, r):
  r = swap(n & 1024, r, Map_/Free)
  r = swap(n & 2048, r, Map_/Free)
  r = swap(n & 4096, r, Map_/Free)
  r = swap(n & 8192, r, Map_/Free)
  r = swap(n & 16384, r, Map_/Free)
  r = swap(n & 32768, r, Map_/Free)
  r = swap(n & 65536, r, Map_/Free)
  r = swap(n & 131072, r, Map_/Free)
  r = swap(n & 262144, r, Map_/Free)
  r = swap(n & 524288, r, Map_/Free)
  return radix3(n, r)

def radix3(n, r):
  r = swap(n & 1048576, r, Map_/Free)
  r = swap(n & 2097152, r, Map_/Free)
  r = swap(n & 4194304, r, Map_/Free)
  r = swap(n & 8388608, r, Map_/Free)
  return r

def reverse(t):
  match t:
    case Arr/Null:
      return Arr/Null
    case Arr/Leaf:
      return t
    case Arr/Node:
      return Arr/Node{ a: reverse(t.b), b: reverse(t.a) }

# Sum : Arr -> U60
def sum(t):
  match t:
    case Arr/Null:
      return 0
    case Arr/Leaf:
      return t.a
    case Arr/Node:
      return sum(t.a) + sum(t.b)

# Gen : U60 -> Arr
def gen(n):
  return gen_go(n, 0)

def gen_go(n, x):
  switch n:
    case 0:
      return Arr/Leaf{ a: x }
    case _:
      a = x * 2
      b = x * 2 + 1
      return Arr/Node{ a: gen_go(n-1, a), b: gen_go(n-1, b) }

Main = (sum (sort(reverse(gen 4))))
