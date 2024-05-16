# Test that branching statements can end with an assignment
type Tree:
  node { val, ~lft, ~rgt }
  leaf

def main:
  deep = 0

  if deep:
    n = 20
  else:
    n = 2

  bend x = 1, h = 0:
    when h < n:
      y = Tree/node { val: x, lft: fork(2 * x, h + 1), rgt: fork(2 * x + 1, h + 1) }
    else:
      y = Tree/leaf

  fold y:
    case Tree/node:
      z = y.val + y.lft + y.rgt
    case Tree/leaf:
      z = 0

  return z