# borrow witness: a fork whose kids read (cnt~0) and take apart (tail~0)
# one list. Xs.tail returns a field: it keeps rest there and stays borrowed
# (term_peek; Xs hot), so Cross keeps nothing on either path, both tasks see
# a live list, and the join holds it and sinks it after both. Correct at 8
# threads.
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
