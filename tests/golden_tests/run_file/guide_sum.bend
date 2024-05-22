type Tree:
  Node { ~lft, ~rgt }
  Leaf { val }

def sum(tree):
  fold tree:
    case Tree/Node:
      return tree.lft + tree.rgt
    case Tree/Leaf:
      return tree.val

def main:
  tree = Tree/Node {
    lft: Tree/Node { lft: Tree/Leaf { val: 1 }, rgt: Tree/Leaf { val: 2 } },
    rgt: Tree/Node { lft: Tree/Leaf { val: 3 }, rgt: Tree/Leaf { val: 4 } }
  }
  return sum(tree)