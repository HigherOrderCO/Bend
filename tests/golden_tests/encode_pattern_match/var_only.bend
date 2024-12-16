(Foo a b) = λf (f a)
(Foo a b) = b

main = @x match x {
  false: Foo
  true: Bool/False
}
