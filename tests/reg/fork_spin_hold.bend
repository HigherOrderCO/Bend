# A spin nested in a fork argument must keep the parallel join's captures
# alive, even when the serial chain no longer uses them. Dropping ys while
# preparing cnt changes the serial frame's layout; the parallel join then
# reads the wrong slots and drains the frontier without a result.
import Base

type Xs is Data:
  X0{}
  X1{v: U32, rest: Xs}

def Xs.len(+all: Xs) -> U32:
  match all:
    case X0{}:
      0
    case X1{v, rest}:
      (1 + Xs.len(rest) : U32)

def Xs.k(n: Nat, +all: Xs, acc: U32) -> U32:
  match n:
    case 0n:
      acc
    case 1n+p:
      Xs.k(p, all, (acc + 1 : U32))

def cnt(n: Nat) -> U32:
  match n:
    case 0n:
      0
    case 1n+p:
      (1 + cnt(p) : U32)

def Xs.mk(n: Nat, acc: Xs) -> Xs:
  match n:
    case 0n:
      acc
    case 1n++p:
      Xs.mk(p, X1{U32.from_nat(p), acc})

def go(f: Nat, n: Nat, zs: Xs) -> U32:
  ys = Xs.mk(n, X0{})
  a b = Xs.len(ys) cnt(U32.to_nat(Xs.k(f, zs, 0)))
  (a + b : U32)

def Xs.sum(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def other(n: Nat, +ys: Xs) -> U32:
  a = Xs.k(n, ys, 0)
  b = Xs.len(ys)
  Xs.sum(ys, (a + b : U32))

def main() -> IO(Unit):
  IO.print(U32.show(go(5n, 7n, X1{1, X0{}})) ++ " " ++ U32.show(other(3n, X1{1, X0{}})))

#|12 5
