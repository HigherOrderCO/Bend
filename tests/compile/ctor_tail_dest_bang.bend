# Constructor tails under !, on each lane: chain fills one destination all
# the way down, and each of lfork's last kids pushes its Dst~put frame above
# the nested left kids' frames. The fork's join still builds its node after
# both kids.
import Base

type Tree is Data:
  Leaf{v: U32}
  Node{l: Tree, r: Tree}

type Chain is Data:
  End{}
  Link{v: U32, t: Chain}

def chain(n: Nat, +i: U32) -> Chain:
  match n:
    case 0n:
      End{}
    case 1n+p:
      t = chain(p, (i + 1 : U32))
      Link{i, t}

def csum(c: Chain, +s: U32) -> U32:
  match c:
    case End{}:
      s
    case Link{v, t}:
      csum(t, ((s * 31 : U32) + v : U32))

def leaf(+i: U32) -> Tree:
  Leaf{(i * 5 : U32)}

def lfork(n: Nat, +i: U32) -> Tree:
  match n:
    case 0n:
      Leaf{i}
    case 1n+p:
      l r = lfork(p, (i + 1 : U32)) leaf(i)
      Node{l, r}

def total(t: Tree) -> U32:
  match t:
    case Leaf{v}:
      v
    case Node{l, r}:
      a b = total(l) total(r)
      ((a * 3 : U32) + b : U32)

def build(+n: Nat, +i: U32) -> Tree:
  match n:
    case 0n:
      Leaf{(csum(chain(1500n, i), 0) + total(lfork(1500n, i)) : U32)}
    case 1n+p:
      l r = build!(p, (i * 2 : U32)) build!(p, ((i * 2 : U32) + 1 : U32))
      Node{l, r}

def main() -> IO(Unit):
  IO.print(U32.show(total(build!(8n, 0))))

#|696369152
