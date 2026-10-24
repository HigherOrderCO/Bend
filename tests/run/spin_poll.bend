# A native spin polls the error word every 4096 steps, not every step: a spin
# that runs past the poll's period must still answer, and answer the same
# (#1776).
import Base

def loop(n: Nat, +i: U32, acc: U32) -> U32:
  match n:
    case 0n: acc
    case 1n+q: loop(q, i, U32.add(acc, 1))

def tree(d: Nat, +n: Nat, +i: U32) -> U32:
  match d:
    case 0n: loop(n, i, 0)
    case 1n++q:
      a b = tree(q, n, i) tree(q, n, i)
      U32.add(a, b)

def main() -> IO(Unit):
  IO.print(U32.show(tree!(4n, 10000n, 0)))

#|160000
