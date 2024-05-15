// Sorts a list

// sort : List -> List
(Sort [])         = []
(Sort (List/Cons x xs)) = (Insert x (Sort xs))

// Insert : U60 -> List -> List
(Insert v [])          = (List/Cons v [])
(Insert v (List/Cons x xs)) = (SwapGT (> v x) v x xs)

// SwapGT : U60 -> U60 -> U60 -> List -> List
(SwapGT 0 v x xs) = (List/Cons v (List/Cons x xs))
(SwapGT _ v x xs) = (List/Cons x (Insert v xs))

// Generates a random list
(Rnd 0 s) = List/Nil
(Rnd n s) =
  let s = (^ s (* s 0b10000000000000))
  let s = (^ s (/ s 0b100000000000000000))
  let s = (^ s (* s 0b100000))
  (List/Cons s (Rnd (- n 1) s))

// Sums a list
(Sum [])         = 0
(Sum (List/Cons x xs)) = (+ x (Sum xs))

(Main) =
  let n = 10
  (Sum (Sort (Rnd 0x100 n)))

// Use an argument from cli
// (Main n) = (Sum (Sort (Rnd 0x100 n)))
