type Bool = True | False

And (Bool/True, Bool/True, Bool/True) = Bool/True
And * = Bool/False

main = (And (Bool/False, Bool/True, Bool/False))
