# should return (a+b+c) * 2^depth
main =
  let depth = 2
  let a = 1;
  let b = 2;
  let c = 3;
  let n = 0;
  let tree = bend n {
    when (< n depth):
      ![(fork (+ n 1)) (fork (+ n 1))]
    else:
      !c
  }
  fold tree {
    Tree/Node: (+ tree.left tree.right)
    Tree/Leaf: (+ tree.value (+ a b))
  }
