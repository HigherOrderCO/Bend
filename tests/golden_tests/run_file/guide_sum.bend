def sum(tree):
  fold tree:
    case Tree/Node:
      return tree.left + tree.right
    case Tree/Leaf:
      return tree.value

def main:
  tree = ![![!1, !2], ![!3, !4]]
  return sum(tree)