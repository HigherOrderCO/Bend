# This program allocates a tree with True at the leaves then parallel ANDs them.
type Bool:
  True
  False

type Tree:
  Node { ~lft, ~rgt }
  Leaf { val }

def and(a, b):
  match a:
    case Bool/True:
      return b
    case Bool/False:
      return Bool/False

def all(tree):
  fold tree:
    case Tree/Node:
      return and(tree.lft, tree.rgt)
    case Tree/Leaf:
      return tree.val

def gen(n):
  switch n:
    case 0:
      return Tree/Leaf(Bool/True)
    case _:
      return Tree/Node { lft: gen(n-1), rgt: gen(n-1) }

def main():
  return all(gen(8))
