type MyTree t = Leaf | (Node ~(lft: (MyTree t)) (val: t) ~(rgt: (MyTree t)))

# Parallel QuickSort
(Sort) : (List u24) -> (MyTree u24)
(Sort  List/Nil)             = MyTree/Leaf
(Sort (List/Cons head tail)) =
  let (min, max) = (Part head tail)
  let lft = (Sort min)
  let rgt = (Sort max)
  (MyTree/Node lft head rgt)

# Partitions a list in two halves, less-than-p and greater-than-p
(Part) : u24 -> (List u24) -> ((List u24), (List u24))
(Part p List/Nil)              = (List/Nil, List/Nil)
(Part p (List/Cons head tail)) = (Push (> head p) head (Part p tail))

# Pushes a value to the first or second list of a pair
(Push) : u24 -> u24 -> ((List u24), (List u24)) -> ((List u24), (List u24))
(Push 0 x (min, max)) = ((List/Cons x min), max)
(Push _ x (min, max)) = (min, (List/Cons x max))

# Generates a random list with xorshift
(Rnd) : u24 -> (u24) -> (List u24)
(Rnd 0 state) = List/Nil
(Rnd n state) =
  let state = (^ state (<< state 13))
  let state = (^ state (>> state 17))
  let state = (^ state (<< state 5))
  (List/Cons state (Rnd (- n 1) state))

# Sums all elements in a concatenation tree
(Sum) : (MyTree u24) -> u24
(Sum MyTree/Leaf)               = 0
(Sum (MyTree/Node lft val rgt)) = (+ val (+ (Sum lft) (Sum rgt)))

# Sorts and sums n random numbers
(Main) : u24 =
  (Sum (Sort (Rnd 0x100 1)))

# Use an argument from cli
# (Main n) = (Sum (Sort (Rnd (<< 1 n) 1)))
