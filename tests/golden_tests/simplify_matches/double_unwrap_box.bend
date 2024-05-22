# Should notice that the second rule is redundant, not create flattened rule for it and not forward the second argument.
type Boxed = (Box x)

(DoubleUnbox (Boxed/Box (Boxed/Box x)) *) = x
(DoubleUnbox * x) = x

Main = (DoubleUnbox (Boxed/Box (Boxed/Box 0)) 5)
