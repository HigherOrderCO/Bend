# io (io_marshal_nat_pair): a Nat in a pair crosses the effect seam as
# a number at any depth, the same as a Nat on its own. Both effects
# answer its typeof.
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

#|number
#|number
