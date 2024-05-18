fib x =
  bend x a=1 b=1 {
     when (!= x 0): (fork (- x 1) b (+ a b))
     else: a
  }

main = (fib 30)
