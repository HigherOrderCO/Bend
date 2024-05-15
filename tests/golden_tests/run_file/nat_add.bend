(Nat/add (Nat/Zero)   x) = x
(Nat/add (Nat/Succ p) x) = (Nat/Succ (Nat/add p x))

(Main) = (Nat/add #25 #9)
