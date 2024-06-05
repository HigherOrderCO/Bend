# This program allocates a tree with True at the leaves then parallel ANDs them.
type Bool:
  True
  False

def and(a, b):
  match a:
    case Bool/True:
      return b
    case Bool/False:
      return Bool/False

def all(tree):
  fold tree:
    case Tree/Node:
      return and(tree.left, tree.right)
    case Tree/Leaf:
      return tree.value

def gen(n):
  switch n:
    case 0:
      return Tree/Leaf(Bool/True)
    case _:
      return Tree/Node { left: gen(n-1), right: gen(n-1) }

def main():
  return all(gen(8))
