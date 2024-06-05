type Error = Err

# Atomic Swapper
(Swap n a b) = switch n {
  0: (Tree/Node a b)
  _: (Tree/Node b a)
}

# Swaps distant values in parallel; corresponds to a Red Box
(Warp s (Tree/Leaf a)   (Tree/Leaf b))   = (Swap (^ (> a b) s) (Tree/Leaf a) (Tree/Leaf b))
(Warp s (Tree/Node a b) (Tree/Node c d)) = (Join (Warp s a c) (Warp s b d))
(Warp s a b) = Error/Err

# Rebuilds the warped tree in the original order
(Join (Tree/Node a b) (Tree/Node c d)) = (Tree/Node (Tree/Node a c) (Tree/Node b d))
(Join a b) = Error/Err

# Recursively warps each sub-tree; corresponds to a Blue/Green Box
(Flow s (Tree/Leaf a))   = (Tree/Leaf a)
(Flow s (Tree/Node a b)) = (Down s (Warp s a b))

# Propagates Flow downwards
(Down s (Tree/Leaf a))   = (Tree/Leaf a)
(Down s (Tree/Node a b)) = (Tree/Node (Flow s a) (Flow s b))

# Bitonic Sort
(Sort s (Tree/Leaf a))   = (Tree/Leaf a)
(Sort s (Tree/Node a b)) = (Flow s (Tree/Node (Sort 0 a) (Sort 1 b)))

# Generates a tree of depth `n`
(Gen n x) = switch n {
  0: (Tree/Leaf x)
  _: (Tree/Node (Gen n-1 (* x 2)) (Gen n-1 (+ (* x 2) 1)))
}

# Reverses a tree
(Rev (Tree/Leaf x))   = (Tree/Leaf x)
(Rev (Tree/Node a b)) = (Tree/Node (Rev b) (Rev a))

# Sums a tree
(Sum (Tree/Leaf x))   = x
(Sum (Tree/Node a b)) = (+ (Sum a) (Sum b))

Main = (Sum (Sort 0 (Rev (Gen 4 0))))
