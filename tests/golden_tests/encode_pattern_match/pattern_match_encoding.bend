data MyType = (A a) | (B b) | (C c) | (D d1 d2) | (E e1 e2)

Foo (MyType/A a) = 100
Foo * = 200

main = (Foo MyType/A 2)
