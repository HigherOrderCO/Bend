# borrow witness: a fork whose kids read (cnt~0 borrowed) and consume
# (push~0 owned: a field takes it) one unsealed list. In sequence, the
# step after cnt keeps the list for push, which wraps the raw word; the
# frame slot the join pops must take the wrapped word too, else the
# join's sink frees the cell b still points at and the free list loops
# (a hang at --threads 1). The tree runs its leaves in sequence at up
# to 128 threads.
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

def Xs.cnt(xs: Xs, acc: U32) -> U32:
  match xs:
    case X0{}:
      acc
    case X1{v, rest}:
      Xs.cnt(rest, (acc + 1 : U32))

def Xs.push(xs: Xs) -> Xs:
  X1{0, xs}

def Push(+xs: Xs) -> U32:
  a b = Xs.cnt(xs, 0) Xs.push(xs)
  (a + Xs.cnt(b, 0) : U32)

def Tree(+d: Nat) -> U32:
  match d:
    case 0n:
      Push(Xs.build(100n, 1))
    case 1n+p:
      a b = Tree(p) Tree(p)
      (a + b : U32)

def main() -> IO(Unit):
  IO.print(U32.show(Tree(12n)))

#|823296
