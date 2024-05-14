data Foo = (CtrA a b) | (CtrB a)
data Bar = (CtrB1 b) | (CtrB2 a b)
data Baz = CtrC

(Rule (Foo/CtrA a (Bar/CtrB1 b))) = (a b)
(Rule (Foo/CtrA a (Bar/CtrB2 (Baz/CtrC) b))) = (a b)
(Rule (Foo/CtrA a b)) = (a b)
(Rule (Foo/CtrB b)) = b
(Rule x) = x
