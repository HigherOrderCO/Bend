type MyTree = Leaf | (Node lft val rgt)

# Parallel QuickSort
(Sort List/Nil)         = MyTree/Leaf
(Sort (List/Cons head tail)) =
  ((Part head tail) λmin λmax
    let lft = (Sort min)
    let rgt = (Sort max)
    (MyTree/Node lft head rgt))

  # Partitions a list in two halves, less-than-p and greater-than-p
  (Part p List/Nil)              = λt (t List/Nil List/Nil)
  (Part p (List/Cons head tail)) = (Push (> head p) head (Part p tail))

  # Pushes a value to the first or second list of a pair
  (Push 0 x pair) = (pair λmin λmax λp (p (List/Cons x min) max))
  (Push _ x pair) = (pair λmin λmax λp (p min (List/Cons x max)))

# Generates a random list with xorshift
(Rnd 0 state) = List/Nil
(Rnd n state) =
  let state = (^ state (<< state 13))
  let state = (^ state (>> state 17))
  let state = (^ state (<< state 5))
  (List/Cons state (Rnd (- n 1) state))

# Sums all elements in a concatenation tree
(Sum MyTree/Leaf)               = 0
(Sum (MyTree/Node lft val rgt)) = (+ val (+ (Sum lft) (Sum rgt)))

# Sorts and sums n random numbers
(Main) =
  (Sum (Sort (Rnd 0x100 1)))

# Use an argument from cli
# (Main n) = (Sum (Sort (Rnd (<< 1 n) 1)))
