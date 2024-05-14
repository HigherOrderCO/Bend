(Nat/add (Nat/zero)   x) = x
(Nat/add (Nat/succ p) x) = (Nat/succ (Nat/add p x))

main = (Nat/add #4 0)
