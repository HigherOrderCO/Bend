type Tree:
  Node { ~lft, ~rgt }
  Leaf { val }

def enum(tree):
  idx = 0
  fold tree with idx:
    case Tree/Node:
      return Tree/Node {
        lft: tree.lft(idx * 2 + 0),
        rgt: tree.rgt(idx * 2 + 1),
      }
    case Tree/Leaf:
      return Tree/Leaf { val: (idx, tree.val) }

def main:
  tree = Tree/Node {
    lft: Tree/Node { lft: Tree/Leaf { val: 1 }, rgt: Tree/Leaf { val: 2 }, },
    rgt: Tree/Node { lft: Tree/Leaf { val: 3 }, rgt: Tree/Leaf { val: 4 }, }
  }
  return enum(tree)