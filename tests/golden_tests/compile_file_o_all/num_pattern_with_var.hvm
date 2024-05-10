// We had a bug where this gave the incorrect error: "Expected a number but found 'a' at definition 'Foo'."
data bool = false | true

(Foo bool/false a) = 0
(Foo bool/true 0) = 0
(Foo bool/true n) = n

Main = (Foo bool/true 3)
