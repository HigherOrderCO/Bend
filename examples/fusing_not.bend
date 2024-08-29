# Example of a boolean 'not' function that fuses inifinitely through composition..

true  : a -> b -> a = λt λf t
false : a -> b -> b = λt λf f

not : ((a -> b -> b) -> (c -> d -> c) -> e) -> e = 
	λboolean (boolean false true)

fusing_not : (b -> a -> c) -> a -> b -> c = 
	λboolean λt λf (boolean f t)

# Creates a Church numeral out of a native number
to_church (n: u24) : (a -> a) -> a -> a = switch n {
	0: λf λx x
	_: λf λx (f (to_church n-1 f x))
}
main: _ = 
	# Self-composes `not` 2^23-1 times and prints the result.
	((to_church 0x7FFFF) fusing_not)  # try replacing 'fusing_not' by 'not'. Will it still work?
