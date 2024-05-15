Tree/Leaf = λnode λleaf leaf
Tree/Node = λval λlft λrgt λnode λleaf (node val lft rgt)

Tree/gen n x = switch n {
  0: Tree/Leaf
  _: (Tree/Node x (Tree/gen n-1 (+ (* x 2) 1)) (Tree/gen n-1 (+ (* x 2) 2)))
}

main = (Tree/gen 4 1)
