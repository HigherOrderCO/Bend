// a binary tree
type MyTree:
  Node { val, ~left, ~right }
  Leaf { val }

// sums all values in a tree
def sum(tree):
  fold tree:
    case MyTree/Node:
      return tree.val + tree.left + tree.right
    case MyTree/Leaf:
      return tree.val

// generates a binary tree of given depth
def gen(depth):
  bend val = 0:
    when val < depth:
      tree = MyTree/Node { val: val, left: go(val+1), right: go(val+1) }
    else:
      tree = MyTree/Leaf { val: val }
  return tree

// returns the sum of 0..2^16
def main:
  return sum(gen(16))