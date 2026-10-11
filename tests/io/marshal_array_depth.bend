# io (io_marshal_array_depth): nested Array<Tree> marshalling must use a
# bounded host stack in both directions, preserve every constructor and
# BigInt Nat field, and return the modified deep leaf to Bend.
import Base

type Tree is Type:
  Leaf{n: Nat}
  Node{n: Nat, kids: Array<Tree>}

law exchange:
  for tree: Tree
  IO(Tree)

def exchange(tree):
  import "./marshal_array_depth.js"
  import "./marshal_array_depth.c"

def grow(n: Nat, tree: Tree) -> Tree:
  match n:
    case 0n:
      tree
    case 1n+p:
      grow(p, Node{123456789n, ALeaf{tree}})

def verify(n: Nat, tree: Tree, valid: Bool) -> Bool:
  match n tree:
    case 0n Leaf{value}:
      Bool.and(valid, Nat.is_eq(value, 987654321n))
    case 1n+p Node{value, kids}:
      match kids:
        case ALeaf{child}:
          verify(p, child, Bool.and(valid, Nat.is_eq(value, 123456789n)))
        case ANode{left, right}:
          False{}
    case _ _:
      False{}

def main() -> IO(Unit):
  do IO<Unit>:
    tree : Tree <- exchange(grow(20000n, Leaf{123456789n}))
    IO.print(Bool.pick(String, verify(20000n, tree, True{}), "ok", "bad"))

#|ok
