# The flattened rule should only have 1 arg, for matching 'B'
type A_t = (A b)
type B_t = B

(Foo 0 (A_t/A B_t/B)) = B_t/B
(Foo * *) = *

main = (Foo 2 (A_t/A B_t/B))
