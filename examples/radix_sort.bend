type MyMap:
  Free
  Used
  Both { ~a: MyMap, ~b: MyMap }

type Arr(t):
  Null
  Leaf { a: t }
  Node { ~a: Arr(t), ~b: Arr(t) }

def swap(s: u24, a: MyMap, b: MyMap) -> MyMap:
  switch s:
    case 0:
      return MyMap/Both{ a: a, b: b }
    case _:
      return MyMap/Both{ a: b, b: a }
 

def sort(t: Arr(u24)) -> Arr(u24):
  return to_arr(0, to_map(t))

def to_map(t: Arr(u24)) -> MyMap:
  match t:
    case Arr/Null:
      return MyMap/Free
    case Arr/Leaf:
      return radix(t.a)
    case Arr/Node:
      return merge(to_map(t.a), to_map(t.b))

def to_arr(x: u24, m: MyMap) -> Arr(u24):
  match m:
    case MyMap/Free:
      return Arr/Null
    case MyMap/Used:
      return Arr/Leaf{ a: x }
    case MyMap/Both:
      return Arr/Node{ a: to_arr(x * 2, m.a), b: to_arr(x * 2 + 1, m.b) }

def merge(a: MyMap, b: MyMap) -> MyMap:
  match a:
    case MyMap/Free:
      return b
    case MyMap/Used:
      return MyMap/Used
    case MyMap/Both:
      match b:
        case MyMap/Free:
          return a
        case MyMap/Used:
          return MyMap/Used
        case MyMap/Both:
          return MyMap/Both{ a: merge(a.a, b.a), b: merge(a.b, b.b) }
  
def radix(n: u24) -> MyMap:
  r = MyMap/Used
  r = swap(n & 1, r, MyMap/Free)
  r = swap(n & 2, r, MyMap/Free)
  r = swap(n & 4, r, MyMap/Free)
  r = swap(n & 8, r, MyMap/Free)
  r = swap(n & 16, r, MyMap/Free)
  r = swap(n & 32, r, MyMap/Free)
  r = swap(n & 64, r, MyMap/Free)
  r = swap(n & 128, r, MyMap/Free)
  r = swap(n & 256, r, MyMap/Free)
  r = swap(n & 512, r, MyMap/Free)
  return radix2(n, r)

# At the moment, we need to manually break very large functions into smaller ones
# if we want to run this program on the GPU.
# In a future version of Bend, we will be able to do this automatically.
def radix2(n: u24, r: MyMap) -> MyMap:
  r = swap(n & 1024, r, MyMap/Free)
  r = swap(n & 2048, r, MyMap/Free)
  r = swap(n & 4096, r, MyMap/Free)
  r = swap(n & 8192, r, MyMap/Free)
  r = swap(n & 16384, r, MyMap/Free)
  r = swap(n & 32768, r, MyMap/Free)
  r = swap(n & 65536, r, MyMap/Free)
  r = swap(n & 131072, r, MyMap/Free)
  r = swap(n & 262144, r, MyMap/Free)
  r = swap(n & 524288, r, MyMap/Free)
  return radix3(n, r)

def radix3(n: u24, r: MyMap) -> MyMap:
  r = swap(n & 1048576, r, MyMap/Free)
  r = swap(n & 2097152, r, MyMap/Free)
  r = swap(n & 4194304, r, MyMap/Free)
  r = swap(n & 8388608, r, MyMap/Free)
  return r

def reverse(t: Arr(u24)) -> Arr(u24):
  match t:
    case Arr/Null:
      return Arr/Null
    case Arr/Leaf:
      return t
    case Arr/Node:
      return Arr/Node{ a: reverse(t.b), b: reverse(t.a) }

def sum(t: Arr(u24)) -> u24:
  match t:
    case Arr/Null:
      return 0
    case Arr/Leaf:
      return t.a
    case Arr/Node:
      return sum(t.a) + sum(t.b)

def gen(n: u24) -> Arr(u24):
  return gen_go(n, 0)

def gen_go(n: u24, x: u24) -> Arr(u24):
  switch n:
    case 0:
      return Arr/Leaf{ a: x }
    case _:
      a = x * 2
      b = x * 2 + 1
      return Arr/Node{ a: gen_go(n-1, a), b: gen_go(n-1, b) }

Main: u24 = (sum (sort(reverse(gen 4))))
