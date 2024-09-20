# Generates a tree with numbers in the nodes using 'bend'
type MyTree(t):
  Node { val: t, ~left: MyTree(t), ~right: MyTree(t) }
  Leaf

def tree_gen(n: u24, x: u24) -> MyTree(u24):
  bend n, x:
    when n != 0:
      return MyTree/Node(x, fork(n - 1, x*2+1), fork(n - 1, x*2+2))
    else:
      return MyTree/Leaf

def main -> MyTree(u24):
  depth = 4
  first_val = 1
  return tree_gen(depth, first_val)

