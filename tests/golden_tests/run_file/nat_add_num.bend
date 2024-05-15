(Nat/add (Nat/Zero)   x) = x
(Nat/add (Nat/Succ p) x) = (Nat/Succ (Nat/add p x))

main = (Nat/add #4 0)
