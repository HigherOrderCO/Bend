// Check that the ctr in the middle are interpreted correctly
data tup = (pair a b)

main = (List/cons
  (String/cons 'a' (tup/pair 'b' (String/cons 'c' String/nil)))
  (List/cons 1 (tup/pair 2 (List/cons 3 (List/cons 4 List/nil))))
)
