// should return (a+b+c) * 2^depth
data Tree = (node ~lft ~rgt) | (leaf val)

main =
  let depth = 2
  let a = 1;
  let b = 2;
  let c = 3;
  let n = 0;
  let tree = bend n {
    when (< n depth):
    (Tree/node (go (+ n 1)) (go (+ n 1)))
  else:
    (Tree/leaf c)
  }
  fold tree {
    Tree/node: (+ tree.lft tree.rgt)
    Tree/leaf: (+ tree.val (+ a b))
  }
