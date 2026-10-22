# io pin (io_reuse_across_effect): bend3 pinned "a lambda owns what it
# captures" across an escaping IO continuation -- a value read again
# in a LATER do-statement lives inside a closure the driver runs after
# the minting body returned. bend's IO surface exists (IO.print is a
# foreign def), so the block checks -- and dies on the affine wall:
# t and xs are plain binders read by two do-statements, and the usage
# cap never switches off. The pure builders port faithfully: depth
# rides + Nat, the doubled reads ride + binders (U32 is Data), strict.
import Base

type Tree is Data:
  Leaf{v: U32}
  Node{l: Tree, r: Tree}

law build:
  for +d: Nat
  for +x: U32
  Tree

def build(d, x):
  match d:
    case 0n:
      Leaf{x}
    case 1n+e:
      Node{build(e, (x + x : U32)), build(e, (x + x + 1 : U32))}

law tsum:
  for t: Tree
  U32

def tsum(t):
  match t:
    case Leaf{v}:
      v
    case Node{l, r}:
      (tsum(l) + tsum(r) : U32)

law lsum:
  for xs: List<&2, U32>
  U32

def lsum(xs):
  match xs:
    case Nil{}:
      0
    case h <> t:
      (h + lsum(t) : U32)

law main:
  IO(Unit)

def main():
  t = build(3n, 1)
  xs = {[1, 2, 3] : List<&2, U32>}
  do IO<Unit>:
    a : Unit <- IO.print(U32.show(tsum(t)))
    b : Unit <- IO.print(U32.show(tsum(t)))
    c : Unit <- IO.print(U32.show(lsum(xs)))
    d : Unit <- IO.print(U32.show(lsum(xs)))
    return Unit{}

#|SOME PROOFS FAIL
#|Error:
#|- message  : xs appears 2 times (at 58:40, 59:40), but a plain binding is used at most once
#|Context:
#|- t : Tree
#|Location: main
#|58 |     c : Unit <- IO.print(U32.show(lsum(xs)))
#|59>|     d : Unit <- IO.print(U32.show(lsum(xs)))
#|   |                                        ^^
#|60 |     return Unit{}
#|Note: a +xs binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
