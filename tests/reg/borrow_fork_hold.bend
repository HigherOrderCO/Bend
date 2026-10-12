# Two different last fork readers of one list must both observe every
# element while safely releasing their references. The join has no later
# use of the list; neither reader may free beneath its sibling.
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

def Xs.len(xs: Xs) -> U32:
  match xs:
    case X0{}:
      0
    case X1{v, rest}:
      (Xs.len(rest) + 1 : U32)

def Xs.sum(xs: Xs, +acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def Xs.done(a: U32, b: U32) -> U32:
  (a + b : U32)

def Twice(+xs: Xs) -> U32:
  a b = Xs.len(xs) Xs.sum(xs, 0)
  Xs.done(a, b)

def main() -> IO(Unit):
  IO.print(U32.show(Twice(Xs.build(2000n, 1))))

#|2003000
