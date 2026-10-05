# literal callbacks passed to defs whose cycle passes no lambda become
# specialized defs; a callee that wraps its callback on recursion keeps
# its closure (wrap), and an erased lambda argument is left alone
import Base

def even?(n: Nat, k: U32 -> U32) -> U32:
  match n:
    case 0n:
      k(0)
    case Succ{p}:
      odd(p, k)

def odd?(n: Nat, k: U32 -> U32) -> U32:
  match n:
    case 0n:
      k(1)
    case Succ{p}:
      even(p, k)

def wrap(n: Nat, f: U32 -> U32) -> U32:
  match n:
    case 0n:
      f(1)
    case Succ{p}:
      wrap(p, y => f(U32.add(y, 2)))

def ignore(x: U32, callback: U32 -> U32) -> U32:
  x

def dapply(P: U32 -> Type, x: U32) -> U32:
  x

def with_pair(callback: U32 -> U32 -> U32, result: U32 & U32) -> U32:
  (a, b) = result
  callback(a, b)

def two(x: U32, f: U32 -> U32, g: U32 -> U32) -> U32:
  g(f(x))

def keep(x: U32, f: U32 -> Array<U32>) -> Array<U32>:
  f(x)

def tag(x: U32, f: U32 -> List<U32>) -> List<U32>:
  f(x)

def snd(r: Array<U32> & U32) -> U32:
  (a, v) = r
  v

def len(xs: List<U32>) -> U32:
  match xs:
    case []:
      0
    case h <> t:
      (1 + len(t) : U32)

def run(+bias: U32, +x: U32, a: Array<U32>, l: List<U32>) -> U32 & U32 & U32 & U32 & U32 & U32 & U32 & U32:
  e = even(5n, y => U32.add(y, bias))
  w = wrap(3n, y => U32.add(y, bias))
  i = ignore(x, y => U32.add(y, bias))
  d = dapply(y => U32, x)
  p = with_pair(y => z => U32.add(U32.add(y, z), bias), (x, 2))
  t = two(x, y => U32.add(y, bias), y => U32.mul(y, bias))
  b = keep(x, y => Array.set(U32, a, 0, U32.add(y, bias)))
  v = snd(Array.get(U32, b, 0))
  n = tag(x, y => y <> l)
  (e, w, i, d, p, t, v, len(n))

def main() -> U32 & U32 & U32 & U32 & U32 & U32 & U32 & U32:
  run(3, 10, Array.new(U32, 2n, 7), [1, 2, 3])

#|(4, 10, 10, 10, 15, 39, 13, 4)
