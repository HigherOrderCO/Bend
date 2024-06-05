# Test that branching statements can end with an assignment
type MyTree:
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
      y = MyTree/node { val: x, lft: fork(2 * x, h + 1), rgt: fork(2 * x + 1, h + 1) }
    else:
      y = MyTree/leaf

  fold y:
    case MyTree/node:
      z = y.val + y.lft + y.rgt
    case MyTree/leaf:
      z = 0

  return z