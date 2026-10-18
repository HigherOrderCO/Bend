# io (io_foreign_types): a foreign def's C lays values of the program's
# own types. The program builds no Tree and names no Empty (its sum has
# a default arm), and never takes a Chain apart at all: it hands one to a
# def that drops it. The emitter defined a CID_ macro for the
# constructors compiled Bend code named, so the C's CID_Empty, CID_End
# and CID_Cell were undefined and it did not compile. Every constructor
# of every datatype now has its macro and its arity, by which the
# default arm and the drop free what they never name.
import Base

type Tree is Data:
  Empty{}
  Leaf{v: U32}
  Node{l: Tree, r: Tree}

type Chain is Data:
  End{}
  Cell{v: U32, next: Chain}

law tree.make:
  U32 -> IO(Tree)

def tree.make(depth):
  import "./foreign_types.c"
  import "./foreign_types.js"

law chain.make:
  U32 -> IO(Chain)

def chain.make(n):
  import "./foreign_types.c"
  import "./foreign_types.js"

def tree.sum(t: Tree) -> U32:
  match t:
    case Leaf{v}:
      v
    case Node{l, r}:
      (tree.sum(l) + tree.sum(r) : U32)
    case other:
      0

def use(c: Chain, +n: U32) -> U32:
  n

def main() -> IO(Unit):
  do IO<Unit>:
    t : Tree <- tree.make(4)
    c : Chain <- chain.make(3)
    IO.print(U32.show((tree.sum(t) + use(c, 5) : U32)))

#|77
