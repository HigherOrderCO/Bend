# Tests that `(Foo 0)` is correctly extracted as a combinator, otherwise the file would hang when running
Foo = @x (x (Foo 0) @y (Foo y))

main = (Foo 0)
