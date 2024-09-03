# A cool trick involving unscoped lambdas
# Implements constant-time queues with just lambdas

# Qnew : Queue a
Qnew: _ = λx x

# Qadd : a -> Queue a -> Queue a
Qadd: _ = λx λq λk (q λc (c x k))

# Qrem : Queue a -> Pair a (Queue a)
Qrem = λq (q $k λx λxs λp(p x λ$k xs))

# Output: [1, 2, 3]
main: (List u24) =
  let q = Qnew
  let q = (Qadd 1 q)
  let q = (Qadd 2 q)
  let q = (Qadd 3 q)
  ((Qrem q) λv0 λq
  ((Qrem q) λv1 λq
  ((Qrem q) λv2 λq
  [1, 2, 3])))
