# We want the nested calls in foo to be compiled as redexes written in outer to inner order
# So they should compile to: @foo = root_tree & a ~ ... & b ~ ... & c ~ ...
foo = @x (a (b (c x)))
foo2 = (a (b (c 0)))

bar = (a (b (c 0) (c (d 1))) (b (c (d 2)) (c 3)))
#bar = (
#  (
#    a
#    (
#      (b (c 0))
#      (c (d 1))
#    )
#  )
#  (
#    (
#      b
#      (c (d 2))
#    )
#    (c 3)
#  )
#)

a = @x x
b = @x x
c = @x x
d = @x x

main = (foo foo2)
