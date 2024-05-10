data Tree = (Leaf x) | (Node x0 x1)

add = λa λb (+ a b)

gen = λn switch n {
  0: (Tree/Leaf 1)
  _: (Tree/Node (gen n-1) (gen n-1))
}

sum = λt
  match t {
    Tree/Leaf: t.x
    Tree/Node: (add (sum t.x0) (sum t.x1))
  }

main = (sum (gen 8))
