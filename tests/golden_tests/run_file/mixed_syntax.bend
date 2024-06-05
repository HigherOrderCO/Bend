type Bool:
  True
  False

type MyTree = (node ~lft ~rgt) | (leaf val)

def tree_xor(tree):
  fold tree:
    case MyTree/node:
      return xor(tree.lft, tree.rgt);
    case MyTree/leaf:
      return tree.val;

(xor Bool/True Bool/False) = Bool/True
(xor Bool/False Bool/True) = Bool/True
(xor * *) = Bool/False

main =
  let depth = 10
  let tree = bend n = 0 {
    when (< n depth):
      (MyTree/node (fork (+ n 1)) (fork (+ n 1)))
    else:
      if (% n 2) { (MyTree/leaf Bool/True) } else { (MyTree/leaf Bool/False) }
  }
  (tree_xor tree)
