add = λa λb (+ a b)

gen = λn switch n {
  0: (Tree/Leaf 1)
  _: (Tree/Node (gen n-1) (gen n-1))
}

sum = λt
  match t {
    Tree/Leaf: t.value
    Tree/Node: (add (sum t.left) (sum t.right))
  }

main = (sum (gen 8))
