# size = 1 << 8

Mul = λn λm λs (n (m s))

# Church nat
C2 = λf λx (f (f x))

# Church powers of two
P2 = (Mul C2 C2) # 4
P3 = (Mul C2 P2) # 8
P4 = (Mul C2 P3) # 16
P5 = (Mul C2 P4) # 32
P6 = (Mul C2 P5) # 64
P7 = (Mul C2 P6) # 128
P8 = (Mul C2 P7) # 256

# Booleans
True = λt λf t
False = λt λf f
Not = λb ((b False) True)
Neg = λb λt λf (b f t)

# Negates 'true' 256 times: 'neg' is faster than 'not' because it fuses
Main = (P8 Neg True)
