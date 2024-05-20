type Tree:
  Node { val, ~left, ~right }
  Leaf

def tree_gen(n, x):
  switch n:
    case 0:
      return Tree/Leaf
    case _:
      return Tree/Node(x, tree_gen(n-1, x*2+1), tree_gen(n-1, x*2+2))

main = (tree_gen 4 1)

