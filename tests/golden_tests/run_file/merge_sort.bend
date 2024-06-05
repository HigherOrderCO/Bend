sort (Tree/Leaf v) = (List/Cons v List/Nil)
sort (Tree/Node a b) = (merge (sort a) (sort b))

merge (List/Nil) b = b
merge (List/Cons x xs) (List/Nil) = (List/Cons x xs)
merge (List/Cons x xs) (List/Cons y ys) =
  let t = switch _ = (< x y) {
    0: λaλbλcλt(t c a b)
    _: λaλbλcλt(t a b c)
  }

  let t = (t (List/Cons x) λx(x) (List/Cons y))

  (t λa λb λc (a (merge (b xs) (c ys))))

sum List/Nil = 0
sum (List/Cons h t) = (+ h (sum t))

range n = switch n {
  0: λx (Tree/Leaf x)
  _: λx (Tree/Node (range n-1 (+ (* x 2) 1)) (range n-1 (* x 2)))
}

main = (sum (sort (range 4 0)))
