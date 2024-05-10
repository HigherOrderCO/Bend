#flavor core
data Tree = Leaf | (Node l m r)

// Parallel QuickSort
(Sort List/nil)         = Tree/Leaf
(Sort (List/cons x xs)) =
  ((Part x xs) λmin λmax
    let lft = (Sort min)
    let rgt = (Sort max)
    (Tree/Node lft x rgt))

  // Partitions a list in two halves, less-than-p and greater-than-p
  (Part p List/nil)         = λt (t List/nil List/nil)
  (Part p (List/cons x xs)) = (Push (> x p) x (Part p xs))

  // Pushes a value to the first or second list of a pair
  (Push 0 x pair) = (pair λmin λmax λp (p (List/cons x min) max))
  (Push _ x pair) = (pair λmin λmax λp (p min (List/cons x max)))

// Generates a random list
(Rnd 0 s) = List/nil
(Rnd n s) =
  let s = (^ s (* s 0b10000000000000))
  let s = (^ s (/ s 0b100000000000000000))
  let s = (^ s (* s 0b100000))
  (List/cons s (Rnd (- n 1) s))

// Sums all elements in a concatenation tree
(Sum Tree/Leaf)         = 0
(Sum (Tree/Node l m r)) = (+ m (+ (Sum l) (Sum r)))

// Sorts and sums n random numbers
(Main) =
  (Sum (Sort (Rnd 0x100 1)))

// Use an argument from cli
// (Main n) = (Sum (Sort (Rnd (<< 1 n) 1)))
