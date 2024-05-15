// Check that the ctr in the middle are interpreted correctly
data tup = (pair a b)

main = (List/Cons
  (String/Cons 'a' (tup/pair 'b' (String/Cons 'c' String/Nil)))
  (List/Cons 1 (tup/pair 2 (List/Cons 3 (List/Cons 4 List/Nil))))
)
