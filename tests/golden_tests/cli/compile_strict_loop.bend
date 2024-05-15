A = @a match a {
  List/Cons: (A a.tail)
  List/Nil: 0
}

main = (A [4 4])
