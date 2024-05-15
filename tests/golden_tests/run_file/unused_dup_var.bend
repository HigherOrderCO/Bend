X = λx x

# This leaves (b1 X) with an ERA in the return, but the DUP is kept with an unused var
main = (
  (λa λb let {b1 b2} = b; (a (b1 X) (b2 X)))
  (λa λb b)
)
