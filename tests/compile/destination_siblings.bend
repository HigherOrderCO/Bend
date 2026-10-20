# Each pending sibling owns its arguments and destination. A caller must
# observe a complete tree even when the leaves take different amounts of work.
import Base

type Tree is Data:
  Leaf{value: U32}
  Node{left: Tree, right: Tree}

def delay(n: Nat, x: U32) -> U32:
  match n:
    case 0n:
      x
    case 1n+p:
      delay(p, U32.inc(x))

def build(+n: Nat, +i: U32) -> Tree:
  match n:
    case 0n:
      Leaf{delay(U32.to_nat(U32.and(i, 7)), i)}
    case 1n+p:
      l r = build(p, U32.shl(i)) build(p, U32.inc(U32.shl(i)))
      Node{l, r}

def total(t: Tree) -> U32:
  match t:
    case Leaf{v}:
      v
    case Node{l, r}:
      a b = total(l) total(r)
      U32.add(a, b)

def main() -> IO(Unit):
  t = build!(12n, 0)
  IO.print(U32.show(U32.add(total!(t), 42)))

#|8400938
