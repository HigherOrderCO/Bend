# A grow left every fork-free task to the drain, even one a fork follows,
# so the drain ran that fork and its whole subtree on one thread. The grow
# now runs such a task in place, as it runs inlined code. Two shapes reach
# it: spin, called before spread's fork, and pair's join, which tail-calls
# spin and returns into chain's fork (leaf can fork, so the grow runs
# pair's two leaf calls and reaches the join). Each prints the same fold
# at any thread count.
import Base

def spin(+n: Nat, +acc: U32) -> U32:
  match n:
    case 0n:
      acc
    case 1n+p:
      U32.xor(spin(p, U32.add(U32.mul(acc, 1664525), 1013904223)), 1)

def spread(+d: Nat, +i: U32) -> U32:
  match d:
    case 0n:
      spin(8n, i)
    case 1n+e:
      +t = spin(4n, i)
      x y = spread(e, U32.mul(t, 2)) spread(e, U32.add(U32.mul(t, 2), 1))
      U32.xor(x, y)

def leaf(+d: Nat, +i: U32) -> U32:
  match d:
    case 0n:
      U32.add(U32.mul(i, 1664525), 1013904223)
    case 1n+e:
      x y = leaf(e, i) leaf(e, U32.add(i, 1))
      U32.xor(x, y)

def pair(+i: U32) -> U32:
  x y = leaf(0n, i) leaf(0n, U32.add(i, 1))
  spin(4n, U32.xor(x, y))

def chain(+d: Nat, +i: U32) -> U32:
  match d:
    case 0n:
      spin(8n, i)
    case 1n+e:
      +t = pair(i)
      x y = chain(e, U32.mul(t, 2)) chain(e, U32.add(U32.mul(t, 2), 1))
      U32.add(x, y)

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(U32.show(spread(8n, 1)))
    IO.print(U32.show(chain(6n, 1)))

#|417464320
#|2860191008
