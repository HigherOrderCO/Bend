#flavor core
data Tree = (Leaf val) | (Both lft rgt)

(U60.swap 0 a b) = (Both a b)
(U60.swap n a b) = (Both b a)

// Swaps distant values in parallel; corresponds to a Red Box
(Warp s (Leaf a) (Leaf b)) = (U60.swap (^ (> a b) s) (Leaf a) (Leaf b))
(Warp s (Both a b) (Both c d)) = (Join (Warp s a c) (Warp s b d))
(Warp s (Leaf a) (Both c d)) = (Both (Warp s (Leaf a) c) (Warp s (Leaf a) d))
(Warp s (Both a b) (Leaf c)) = (Both (Warp s a (Leaf c)) (Warp s b (Leaf c)))

// Rebuilds the warped tree in the original order
(Join (Leaf a) (Leaf b)) = (Both a b)
(Join (Leaf a) (Both c d)) = (Both a (Both c d))
(Join (Both a b) (Leaf c)) = (Both (Both a b) c)
(Join (Both a b) (Both c d)) = (Both (Both a c) (Both b d))

// Recursively warps each sub-tree; corresponds to a Blue/Green Box
(Flow s (Leaf a))   = (Leaf a)
(Flow s (Both a b)) = (Down s (Warp s a b))

// Propagates Flow downwards
(Down s (Leaf a))   = (Leaf a)
(Down s (Both a b)) = (Both (Flow s a) (Flow s b))

// Bitonic Sort
(Sort s (Leaf a))   = (Leaf a)
(Sort s (Both a b)) = (Flow s (Both (Sort 0 a) (Sort 1 b)))

// Generates a tree of depth `n`
(Gen 0 x) = (Leaf x)
(Gen n x) = let m = (- n 1); (Both (Gen m (* x 2)) (Gen m (+ (* x 2) 1)))

// Reverses a tree
(Rev (Leaf x))   = (Leaf x)
(Rev (Both a b)) = (Both (Rev b) (Rev a))

// Sums a tree
(Sum (Leaf x))   = x
(Sum (Both a b)) = (+ (Sum a) (Sum b))

(Main) =
  let n = 10
  (Sum (Sort 0 (Rev (Gen n 0))))

// Use an argument from cli
// (Main n) = (Sum (Sort 0 (Rev (Gen n 0))))
