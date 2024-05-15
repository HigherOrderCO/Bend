data Tree = Leaf | (Node l m r)

// Parallel QuickSort
(Sort List/Nil)         = Tree/Leaf
(Sort (List/Cons x xs)) =
  ((Part x xs) λmin λmax
    let lft = (Sort min)
    let rgt = (Sort max)
    (Tree/Node lft x rgt))

  // Partitions a list in two halves, less-than-p and greater-than-p
  (Part p List/Nil)         = λt (t List/Nil List/Nil)
  (Part p (List/Cons x xs)) = (Push (> x p) x (Part p xs))

  // Pushes a value to the first or second list of a pair
  (Push 0 x pair) = (pair λmin λmax λp (p (List/Cons x min) max))
  (Push _ x pair) = (pair λmin λmax λp (p min (List/Cons x max)))

// Generates a random list
(Rnd 0 s) = List/Nil
(Rnd n s) =
  let s = (^ s (* s 0b10000000000000))
  let s = (^ s (/ s 0b100000000000000000))
  let s = (^ s (* s 0b100000))
  (List/Cons s (Rnd (- n 1) s))

// Sums all elements in a concatenation tree
(Sum Tree/Leaf)         = 0
(Sum (Tree/Node l m r)) = (+ m (+ (Sum l) (Sum r)))

// Sorts and sums n random numbers
(Main) =
  (Sum (Sort (Rnd 0x100 1)))

// Use an argument from cli
// (Main n) = (Sum (Sort (Rnd (<< 1 n) 1)))
