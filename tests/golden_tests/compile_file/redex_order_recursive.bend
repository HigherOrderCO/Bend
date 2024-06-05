tail_recursive = @x (x @pred @acc (tail_recursive pred (+ 1 acc)) @acc 0)

fold_ = @bm (bm
  @lft @rgt (add (fold_ lft) (fold_ rgt))
  @s @z (s @s @z z)
)

add = @a (a
  @p @b (add p @s @z (s b))
  @b b
)

List.sum xs acc = match xs {
  List/Nil: acc
  List/Cons: (List.sum xs.tail (+ xs.head acc))
}

List.len xs = match xs {
  List/Nil: 0
  List/Cons: (+ 1 (List.len xs.tail))
}

List.len_tr xs acc = match xs {
  List/Nil: acc
  List/Cons: (List.len_tr xs.tail (+ 1 acc))
}

List.map xs f = match xs {
  List/Nil: List/Nil
  List/Cons: (List/Cons (f xs.head) (List.map xs.tail f))
}

List.concat xs ys = match xs {
  List/Nil: ys
  List/Cons: (List/Cons xs.head (List.concat xs.tail ys))
}

List.reverse_bad xs = match xs {
  List/Nil: List/Nil
  List/Cons: (List.concat (List.reverse_bad xs.tail) (List/Cons xs.head List/Nil))
}

List.reverse_tr xs acc = (List.reverse_over xs List/Nil)

List.fold xs c n = match xs {
  List/Nil: n
  List/Cons: (c xs.head (List.fold xs.tail c n))
}

List.reduce xs f acc = match xs {
  List/Nil: acc
  List/Cons: (List.reduce xs.tail f (f acc xs.head))
}

List.reverse_over xs ys = match xs {
  List/Nil: ys
  List/Cons: (List.reverse_over xs.tail (List/Cons xs.head ys))
}

Tree.leaves tree = match tree {
  Tree/Leaf: 1
  Tree/Node: (+ (Tree.leaves tree.left) (Tree.leaves tree.right))
}

Tree.nodes tree = match tree {
  Tree/Leaf: 0
  Tree/Node: (+ 1 (+ (Tree.nodes tree.left) (Tree.nodes tree.right)))
}

Tree.height tree = match tree {
  Tree/Leaf: 0
  Tree/Node: (+ 1 (max (Tree.height tree.left) (Tree.height tree.right)))
}

Tree.map tree f = match tree {
  Tree/Leaf: (Tree/Leaf (f tree.value))
  Tree/Node: (Tree/Node (Tree.map tree.left f) (Tree.map tree.right f))
}

max a b = switch (> a b) {
  0: b
  _: a
}

Tree.flip tree = match tree {
  Tree/Leaf: tree
  Tree/Node: (Tree/Node (Tree.flip tree.right) (Tree.flip tree.left))
}

calc =
  let n0 = 1
  let n1 = (* n0 n0)
  let n2 = (* n1 n1)
  let n3 = (* n2 n2)
  let n4 = (* n3 n3)
  let n5 = (* n4 n4)
  (Foo (+ n0 n1) (+ n2 n3) (+ n4 n5))
Foo = @a @b @c @x (x a b c)

main = *
