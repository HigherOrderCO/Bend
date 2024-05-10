data Tree = (node ~lft ~rgt) | (leaf val)

(MergeSort tree) = fold tree {
  Tree/node: (merge tree.lft tree.rgt)
  Tree/leaf: tree
}

(Merge (Tree/leaf a) (Tree/leaf b)) =
  let (a, b) = if (<= a b) { (a, b) } else { (b, a) }
  (Tree/node (Tree/leaf a) (Tree/leaf b))


(concat a b) = match a {
  List/cons: 
}