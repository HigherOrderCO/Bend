# io (io_marshal_nat_pair): a Nat in a pair crosses the effect seam as
# a BigInt at any depth. A pair's second half is a type family, and the
# marshal walk took it for no Nat: it found U32 & Nat's through the
# Sigma's fields, but in U32 & U32 & Nat it had seen Sigma already, so
# that Nat crossed as a number. Both effects answer the BigInt's typeof.
import Base

law top:
  for p: U32 & Nat
  IO(String)

def top(p):
  import "./marshal_nat_pair.js"

law deep:
  for p: U32 & U32 & Nat
  IO(String)

def deep(p):
  import "./marshal_nat_pair.js"

law main:
  IO(Unit)

def main():
  do IO<Unit>:
    a : String <- top((1, 5n))
    b : String <- deep((1, (2, 5n)))
    u : Unit <- IO.print(a)
    IO.print(b)

#|bigint
#|bigint
