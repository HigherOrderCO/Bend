# A fork with an unused list argument must release that task's reference
# without invalidating its sibling. Thrice also retains the list for a
# later traversal, so that original owner must survive the join.
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

def Xs.seven(xs: Xs) -> U32:
  7

def Twice(+xs: Xs) -> U32:
  a b = Xs.cnt(xs, 0) Xs.seven(xs)
  (a + b : U32)

def Thrice(+xs: Xs) -> U32:
  a b = Xs.cnt(xs, 0) Xs.seven(xs)
  (a + b + Xs.sum(xs, 0) : U32)

def main() -> IO(Unit):
  IO.print(U32.show((Twice(Xs.build(100n, 1)) + Thrice(Xs.build(10n, 1)) : U32)))

#|179
