true = λt λf t
false = λt λf f
not = λboolean (boolean false true)
fusing_not = λboolean λt λf (boolean f t)
// Creates a Church numeral out of a native number
to_church n = switch n {
	0: λf λx x
	_: λf λx (f (to_church n-1 f x))
}
main = 
	// Self-composes `not` 2^24 times and prints the result.
	((to_church 0xFFFFFF) fusing_not)  // try replacing this by not. Will it still work?
