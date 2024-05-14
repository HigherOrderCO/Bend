data Tree = (Leaf a) | (Both a b)
data Error = Err

// Atomic Swapper
(Swap n a b) = switch n {
  0: (Tree/Both a b)
  _: (Tree/Both b a)
}

// Swaps distant values in parallel; corresponds to a Red Box
(Warp s (Tree/Leaf a)   (Tree/Leaf b))   = (Swap (^ (> a b) s) (Tree/Leaf a) (Tree/Leaf b))
(Warp s (Tree/Both a b) (Tree/Both c d)) = (Join (Warp s a c) (Warp s b d))
(Warp s a b) = Error/Err

// Rebuilds the warped tree in the original order
(Join (Tree/Both a b) (Tree/Both c d)) = (Tree/Both (Tree/Both a c) (Tree/Both b d))
(Join a b) = Error/Err

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
(Gen n x) = switch n {
  0: (Tree/Leaf x)
  _: (Tree/Both (Gen n-1 (* x 2)) (Gen n-1 (+ (* x 2) 1)))
}

// Reverses a tree
(Rev (Tree/Leaf x))   = (Tree/Leaf x)
(Rev (Tree/Both a b)) = (Tree/Both (Rev b) (Rev a))

// Sums a tree
(Sum (Tree/Leaf x))   = x
(Sum (Tree/Both a b)) = (+ (Sum a) (Sum b))

Main = (Sum (Sort 0 (Rev (Gen 4 0))))
