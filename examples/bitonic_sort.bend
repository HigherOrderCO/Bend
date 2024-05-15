data Tree = (Leaf val) | (Both lft rgt)

(U60.swap 0 a b) = (Tree/Both a b)
(U60.swap n a b) = (Tree/Both b a)

// Swaps distant values in parallel; corresponds to a Red Box
(Warp s (Tree/Leaf a) (Tree/Leaf b)) = (U60.swap (^ (> a b) s) (Tree/Leaf a) (Tree/Leaf b))
(Warp s (Tree/Both a b) (Tree/Both c d)) = (Join (Warp s a c) (Warp s b d))
(Warp s (Tree/Leaf a) (Tree/Both c d)) = (Tree/Both (Warp s (Tree/Leaf a) c) (Warp s (Tree/Leaf a) d))
(Warp s (Tree/Both a b) (Tree/Leaf c)) = (Tree/Both (Warp s a (Tree/Leaf c)) (Warp s b (Tree/Leaf c)))

// Rebuilds the warped tree in the original order
(Join (Tree/Leaf a) (Tree/Leaf b)) = (Tree/Both a b)
(Join (Tree/Leaf a) (Tree/Both c d)) = (Tree/Both a (Tree/Both c d))
(Join (Tree/Both a b) (Tree/Leaf c)) = (Tree/Both (Tree/Both a b) c)
(Join (Tree/Both a b) (Tree/Both c d)) = (Tree/Both (Tree/Both a c) (Tree/Both b d))

// Recursively warps each sub-tree; corresponds to a Blue/Green Box
(Flow s (Tree/Leaf a))   = (Tree/Leaf a)
(Flow s (Tree/Both a b)) = (Down s (Warp s a b))

// Propagates Flow downwards
(Down s (Tree/Leaf a))   = (Tree/Leaf a)
(Down s (Tree/Both a b)) = (Tree/Both (Flow s a) (Flow s b))

// Bitonic Sort
(Sort s (Tree/Leaf a))   = (Tree/Leaf a)
(Sort s (Tree/Both a b)) = (Flow s (Tree/Both (Sort 0 a) (Sort 1 b)))

// Generates a tree of depth `n`
(Gen 0 x) = (Tree/Leaf x)
(Gen n x) = let m = (- n 1); (Tree/Both (Gen m (* x 2)) (Gen m (+ (* x 2) 1)))

// Reverses a tree
(Rev (Tree/Leaf x))   = (Tree/Leaf x)
(Rev (Tree/Both a b)) = (Tree/Both (Rev b) (Rev a))

// Sums a tree
(Sum (Tree/Leaf x))   = x
(Sum (Tree/Both a b)) = (+ (Sum a) (Sum b))

(Main) = (Sum (Sort 0 (Rev (Gen 18 0))))

// Use an argument from cli
// (Main n) = (Sum (Sort 0 (Rev (Gen n 0))))
