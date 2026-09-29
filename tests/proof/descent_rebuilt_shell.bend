# --verdict: a self-call on a constructor rebuilt around a smaller part
# of its input (#1167). The argument is no piece of the input, but it is
# smaller: the kernel checks descent by size, so a shell whose fields
# are each no bigger than the input's, one of them smaller, descends.
# height recurses on Node{rest} under Node{Cons{x, rest}}; flat on
# Cons{xs, ys} under Cons{Cons{x, xs}, ys}; drop2 on a piece rebuilt
# whole, Cons{y, t} under Cons{x, Cons{y, t}}; skip on a shell around
# such a piece, Node{Cons{y, r}} under Node{Cons{x, Cons{y, r}}}; dig on
# a shell whose fields sit in two different pieces below, as bend2's
# search finds them.
import Base

type Tree is Data:
  Leaf{}
  Node{children: Forest}

type Forest is Data:
  End{}
  Cons{tree: Tree, rest: Forest}

def height(t: Tree) -> Nat:
  match t:
    case Leaf{}: 0n
    case Node{End{}}: 1n
    case Node{Cons{+x, rest}}: Nat.max(1n+height(x), height(Node{rest}))

def flat(l: List<List<Nat>>) -> List<Nat>:
  match l:
    case []:
      []
    case [] <> ys:
      flat(ys)
    case (x <> xs) <> ys:
      x <> flat(xs <> ys)

def drop2(l: List<Nat>) -> List<Nat>:
  match l:
    case []:
      []
    case x <> []:
      []
    case x <> (y <> t):
      drop2(y <> t)

def skip(t: Tree) -> Nat:
  match t:
    case Leaf{}: 0n
    case Node{End{}}: 0n
    case Node{Cons{+x, End{}}}: 1n
    case Node{Cons{+x, Cons{+y, r}}}: 1n+skip(Node{Cons{y, r}})

def dig(l: List<Nat>) -> Nat:
  match l:
    case Con{Zero{}, Con{Succ{Succ{x}}, Con{Succ{y}, Nil{}}}}:
      1n+dig(Con{x, Nil{}})
    case _:
      0n

def main() -> Nat & List<Nat> & List<Nat> & Nat & Nat:
  (height(Node{Cons{Node{Cons{Leaf{}, End{}}}, Cons{Leaf{}, End{}}}}), flat([[1n, 2n], [], [3n]]), drop2([1n, 2n, 3n]), skip(Node{Cons{Leaf{}, Cons{Leaf{}, Cons{Leaf{}, End{}}}}}), dig([0n, 2n, 1n]))

#|(2n, [1n, 2n, 3n], [], 3n, 1n)
