# borrow witness: mixed conventions in one call. Mixed~1 is owned (F moves
# ys in, so no call lends it) and Mixed~0 borrowed (a is only read):
# F reads xs in place (term_peek) and moves ys, no term_keep in the C.
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

def Mixed(a: Xs, b: Xs) -> U32:
  z = Xs.tail(b)
  Xs.sum(a, Xs.cnt(z, 0))

def F(+xs: Xs, ys: Xs) -> U32:
  r = Mixed(xs, ys)
  (r + Xs.cnt(xs, 0) : U32)

def main() -> IO(Unit):
  IO.print(U32.show(F(Xs.build(3n, 1), Xs.build(2n, 1))))

#|10
