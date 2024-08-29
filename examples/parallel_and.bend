# This program allocates a tree with True at the leaves then parallel ANDs them.
type Bool:
  True
  False

def and(a: Bool, b: Bool) -> Bool:
  match a:
    case Bool/True:
      return b
    case Bool/False:
      return Bool/False

# Trees of tuples are not typeable
def all(tree: Tree(Bool)) -> Bool:
  fold tree:
    case Tree/Node:
      return and(tree.left, tree.right)
    case Tree/Leaf:
      return tree.value

def gen(n: u24) -> Tree(Bool):
  switch n:
    case 0:
      return Tree/Leaf(Bool/True)
    case _:
      return Tree/Node { left: gen(n-1), right: gen(n-1) }

def main() -> Bool:
  return all(gen(8))
