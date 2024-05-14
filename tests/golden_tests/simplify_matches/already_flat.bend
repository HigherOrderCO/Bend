data Foo = CtrA | (CtrB x)
data Bar = (CtrA1 a) | (CtrA2 a1 a2) | (CtrA3 a)
data Baz = (CtrB0) | (CtrB1 b) | (CtrB2 b) | (CtrB3 b)

Rule1 = λx x

Rule2 a = λx x

Rule3 a b c d = (a b c d)

Rule4 (Foo/CtrA) = λx x
Rule4 (Foo/CtrB x) = x
Rule4 x = x

Rule5 (Bar/CtrA1 a) b = (a b)
Rule5 (Bar/CtrA2 a1 a2) b = (a1 a2 b)
Rule5 a (Baz/CtrB0) = a
Rule5 a (Baz/CtrB1 b) = (a b)
Rule5 (Bar/CtrA3 a) (Baz/CtrB3 b) = (a b)
Rule5 a b = (a b)

Rule6 a = a
Rule6 b = b
