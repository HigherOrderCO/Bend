#flavor core
zero = λs λz z
succ = λpred λs λz (s pred) // Creates a Scott number out of its predecessor

two = (succ (succ zero)) // λs λz (s λs λz (s λs λz z))

fusing_add = λa
	let case_succ = λa_pred λb (succ (fusing_add a_pred b))
	let case_zero = λb b
	(a case_succ case_zero)

Main = λx (fusing_add two x)