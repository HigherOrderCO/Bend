# Example of a boolean 'not' function that fuses inifinitely through composition..

true = λt λf t
false = λt λf f

not = λboolean (boolean false true)
fusing_not = λboolean λt λf (boolean f t)

# Creates a Church numeral out of a native number
to_church n = switch n {
	0: λf λx x
	_: λf λx (f (to_church n-1 f x))
}
main = 
	# Self-composes `not` 2^23-1 times and prints the result.
	((to_church 0x7FFFFF) fusing_not)  # try replacing 'fusing_not' by 'not'. Will it still work?
