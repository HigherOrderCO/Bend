# regression witness: a spin called inside a jump's argument, lent a
# variable that dies at the call. Xs.walk and Xs.k borrow their +all (Xs.k
# because other lends it a list it reads after). In go,
# Xs.walk(xs, xs, X0{}) lends xs to both parameters; in go2,
# Xs.k(f, xs, acc) lends the only xs. Either way the lent xs dies at the
# spin call, and the self-jump that takes the result runs no bind_dead, so
# the spin call must sink it. Before, nothing did: every turn left the old
# list's cells: about 81.6 MB -> 1.7 MB max RSS on one C thread with
# --gpu off. The #| lines guard output, not RSS. In the emitted C, each
# spin call is followed by a term_sink of xs.
import Base

type Xs is Data:
  X0{}
  X1{v: U32, rest: Xs}

def Xs.walk(xs: Xs, +all: Xs, acc: Xs) -> Xs:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.walk(rest, all, X1{(v + 1 : U32), acc})

def Xs.sum(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.sum(rest, (acc + v : U32))

def Xs.k(n: Nat, +all: Xs, acc: U32) -> U32:
  match n:
    case 0n:
      acc
    case 1n+p:
      Xs.k(p, all, (acc + 1 : U32))

def go(k: Nat, +xs: Xs) -> Xs:
  match k:
    case 0n:
      xs
    case 1n+p:
      go(p, Xs.walk(xs, xs, X0{}))

def go2(k: Nat, +f: Nat, xs: Xs, +acc: U32) -> U32:
  match k:
    case 0n:
      acc
    case 1n+p:
      go2(p, f, X1{acc, X0{}}, Xs.k(f, xs, acc))

def other(n: Nat, +ys: Xs) -> U32:
  a = Xs.k(n, ys, 0)
  Xs.sum(ys, a)

def main() -> IO(Unit):
  +a = Xs.sum(go(U32.to_nat(1000000), X1{1, X1{2, X1{3, X1{4, X0{}}}}}), 0)
  +b = go2(U32.to_nat(1000000), 2n, X0{}, 0)
  IO.print(U32.show(a) ++ " " ++ U32.show(b) ++ " " ++ U32.show(other(3n, X1{1, X0{}})))

#|4000010 2000000 4
