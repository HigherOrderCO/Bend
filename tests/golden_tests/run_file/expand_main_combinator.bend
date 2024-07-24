# It should reduce to `@a (a 1 2)` if main is expanded correctly.
# (ctr 1 2) should not be extracted from main into a separate function.
main = my_fn
my_fn = (@x x (ctr 1 2))
ctr = @a @b @x (x a b)