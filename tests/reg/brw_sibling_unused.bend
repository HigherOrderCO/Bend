# borrow witness: a sibling dropping an unused parameter. Pick never uses
# ys, and F holds ys across the call: Pick~1 is borrowed, so FID_Pick has
# no term_sink and F has no term_keep; the list dies in F.
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

def Pick(a: U32, ys: Xs) -> U32:
  a

def F(+ys: Xs) -> U32:
  a = Pick(1, ys)
  b = Xs.cnt(ys, 0)
  (a + b : U32)

def main() -> IO(Unit):
  IO.print(U32.show(F(Xs.build(50n, 1))))

#|51
