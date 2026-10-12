# Different fork readers must retain independent ownership, while a returned
# owner remains live through the join. Each reader recursively forks its tree.
import Base

type Tree is Data:
  Leaf{v: U32}
  Node{l: Tree, r: Tree}

def gen(+d: Nat) -> Tree:
  match d:
    case 0n:
      Leaf{5}
    case 1n+p:
      a b = gen(p) gen(p)
      Node{a, b}

def sum(t: Tree) -> U32:
  match t:
    case Leaf{v}:
      v
    case Node{l, r}:
      a b = sum(l) sum(r)
      (a + b : U32)

def count(t: Tree) -> U32:
  match t:
    case Leaf{v}:
      1
    case Node{l, r}:
      a b = count(l) count(r)
      (a + b : U32)

def consume(+t: Tree) -> U32:
  a b = sum(t) count(t)
  (a + b : U32)

def retain(+t: Tree) -> U32 & Tree:
  a b = sum(t) count(t)
  ((a + b : U32), t)

def retained(p: U32 & Tree) -> U32:
  (value, tree) = p
  (value + sum(tree) : U32)

def main() -> IO(Unit):
  kept = retained(retain(gen(10n)))
  do IO<Unit>:
    Unit <- IO.print(U32.show(consume(gen(10n))))
    Unit <- IO.print(U32.show(kept))
    IO.print(U32.show(consume!(gen(10n))))

#|6144
#|11264
#|6144
