T = λt λf t
F = λt λf f
And = λpλq (p q F)

Z = λs λz (z)
S = λn λs λz (s n)

Add = λa λb (a (S b))
Mul = λa λb λf (a (b f))
Pow = λa λb (a (Mul b) (S Z))

Node = λa λb λn λl (n a b)
Leaf = λn λl l

Alloc = λn (n (λp (Node (Alloc p) (Alloc p))) Leaf)
Destroy = λt (t (λaλb (And (Destroy a) (Destroy b))) T)

Main = (Destroy (Alloc (Pow (S (S Z)) (S (S (S (S Z)))))))
