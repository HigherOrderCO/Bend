# borrow witness: one value twice in one call. Both reads its twins, so F
# lends xs to both and keeps nothing (the continuation sinks the list after
# the call). Mixed hands b to Xs.push, which stores it and so owns its
# parameter: Mixed~1 is owned, Mixed~0 borrowed, and G must keep xs (one
# term_keep). A compiler that moves the twin into Mixed~1 has sum walk the
# cells Mixed sank (the free list overwrites v: a wrong sum; a poisoned
# heap_free faults).
import Base

type Xs is Data:
  X0{}
  X1{v: U32, rest: Xs}

def Xs.build(n: Nat, +acc: U32) -> Xs:
  match n:
    case 0n:
      X0{}
    case 1n+p:
      X1{acc, Xs.build(p, (acc + 1 : U32))}

def Xs.sum(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def Xs.cnt(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.cnt(rest, (acc + 1 : U32))

def Xs.push(xs: Xs) -> Xs:
  X1{0, xs}

def Both(a: Xs, b: Xs) -> U32:
  (Xs.cnt(a, 0) + Xs.cnt(b, 0) : U32)

def Mixed(a: Xs, b: Xs) -> U32:
  z = Xs.push(b)
  Xs.sum(a, Xs.cnt(z, 0))

def F(+xs: Xs) -> U32:
  r = Both(xs, xs)
  (r + 1 : U32)

def G(+xs: Xs) -> U32:
  r = Mixed(xs, xs)
  (r + 1 : U32)

def main() -> IO(Unit):
  IO.print(U32.show((F(Xs.build(10n, 1)) + G(Xs.build(1000n, 1)) : U32)))

#|501523
