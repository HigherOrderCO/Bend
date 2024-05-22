# Example of fusing functions with Scott-encoded numbers.

zero = λs λz z
succ = λpred λs λz (s pred) # Creates a Scott number out of its predecessor

two = (succ (succ zero)) # λs λz (s λs λz (s λs λz z))

fusing_add = λa
	let case_succ = λa_pred λb (succ (fusing_add a_pred b))
	let case_zero = λb b
	(a case_succ case_zero)

# (fusing_add two) creates a fused function that efficiently adds two to whatever you pass to it.
Main = λx (fusing_add two x)
