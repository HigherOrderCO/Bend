def enum(tree):
  idx = 0
  fold tree with idx:
    case Tree/Node:
      return ![tree.left(idx * 2 + 0), tree.right(idx * 2 + 1)]
    case Tree/Leaf:
      return !(idx, tree.value)

def main:
  tree = ![![!1, !2], ![!3, !4]]
  return enum(tree)