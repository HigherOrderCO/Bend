# There was a bug where it would say "Non-exhaustive pattern (foo f1 f2 f3 f3)",
# repeating the missing constructor for the next args.

data b1 = f1 | t1
data b2 = f2 | t2
data b3 = f3 | t3
data b4 = f4 | t4

(foo b1/f1 b2/f2 b3/f3 b4/f4) = 0
(foo b1/f1 b2/f2 b3/f3 b4/t4) = 1

(main) = (foo b1/f1 b2/f2 b3/f3 b4/f4)
