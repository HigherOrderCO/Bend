# a parallel let in a def whose last parameter is erased (#1374):
# tree(q, i) before the erased z is the call itself, not a prefix an
# over-application cuts, so the two calls stay one fork (a join in
# the C) instead of two sequential lets. Stdout can't see a fork:
# main prints the same sum, so this runs the forked shape on every
# lane but doesn't catch a revert
import Base

law leaf:
  for i: U32
  U32

def leaf(i):
  (i * 3 + 1 : U32)

law tree:
  for +d: Nat
  for +i: U32
  for -z: Nat
  U32

def tree(d, i, z):
  match d:
    case 0n:
      leaf(i)
    case 1n+q:
      a b = tree(q, (i * 2 : U32), z) tree(q, (i * 2 + 1 : U32), z)
      (a + b : U32)

law main.out:
  U32

def main.out():
  tree(5n, 1, 0n)

law main:
  IO(Unit)

def main():
  IO.print(U32.show(main.out()))

#|4592
