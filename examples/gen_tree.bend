# Generates a tree with numbers in the nodes using 'bend'
type Tree:
  Node { val, ~left, ~right }
  Leaf

def tree_gen(n, x):
  bend n, x:
    when n != 0:
      return Tree/Node(x, fork(n - 1, x*2+1), fork(n - 1, x*2+2))
    else:
      return Tree/Leaf

def main:
  depth = 4
  first_val = 1
  return tree_gen(depth, first_val)

