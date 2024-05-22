type Tree:
  Node { ~lft, ~rgt }
  Leaf { val }

def main():
  bend x = 0:
    when x < 3:
      tree = Tree/Node { lft: fork(x + 1), rgt: fork(x + 1) }
    else:
      tree = Tree/Leaf { val: 7 }
  return tree