(Nat/add (Nat/zero)   x) = x
(Nat/add (Nat/succ p) x) = (Nat/succ (Nat/add p x))

(Main) = (Nat/add #25 #9)
