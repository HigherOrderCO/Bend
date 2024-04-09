add = λa λb (+ a b)

fib = λx switch x {
  0: 1
  _: let p = x-1; switch p {
    0: 1
    _: (+ (fib p) (fib p-1))
  }
}

main = (fib 30)
