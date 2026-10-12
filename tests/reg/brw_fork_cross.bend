# A forked reader and a tail consumer must share a live list safely.
# Returning a field from one task must not invalidate the other task's
# traversal; the returned tail is still readable after the join.
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

def Xs.tail(xs: Xs) -> Xs:
  match xs:
    case X0{}:
      X0{}
    case X1{v, rest}:
      rest

def Cross(+xs: Xs) -> U32:
  a b = Xs.cnt(xs, 0) Xs.tail(xs)
  (a + Xs.cnt(b, 0) : U32)

def main() -> IO(Unit):
  IO.print(U32.show(Cross(Xs.build(100n, 1))))

#|199
