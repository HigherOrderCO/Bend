data Tree = (Leaf x) | (Node x0 x1) 
data List_ = Nil | (Cons h t)

sort (Tree/Leaf v) = (List_/Cons v List_/Nil)
sort (Tree/Node a b) = (merge (sort a) (sort b))

merge (List_/Nil) b = b
merge (List_/Cons x xs) (List_/Nil) = (List_/Cons x xs)
merge (List_/Cons x xs) (List_/Cons y ys) = 
  let t = switch _ = (< x y) {
    0: λaλbλcλt(t c a b)
    _: λaλbλcλt(t a b c)
  }

  let t = (t (List_/Cons x) λx(x) (List_/Cons y))

  (t λa λb λc (a (merge (b xs) (c ys))))

sum List_/Nil = 0
sum (List_/Cons h t) = (+ h (sum t))

range n = switch n {
  0: λx (Tree/Leaf x)
  _: λx (Tree/Node (range n-1 (+ (* x 2) 1)) (range n-1 (* x 2)))
}

main = (sum (sort (range 4 0)))
