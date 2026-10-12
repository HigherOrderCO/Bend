# io (io_marshal_nat_number): a Nat crosses the effect seam as the
# host's number, unconverted, in every lane: the answers 0 and 5 land
# as zero and a successor; the interpreter and compiled JS print the same
# two tags.
import Base

law zero:
  IO(Nat)

def zero():
  import "./marshal_nat_number.js"

law five:
  IO(Nat)

def five():
  import "./marshal_nat_number.js"

law tag_of:
  for n: Nat
  String

def tag_of(n):
  match n:
    case 0n:
      "zero"
    case 1n+p:
      "succ"

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Nat <- zero()
    b : Nat <- five()
    u : Unit <- IO.print(tag_of(a))
    v : Unit <- IO.print(tag_of(b))
    return 0

#|zero
#|succ
