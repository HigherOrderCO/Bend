# I32 beyond its arithmetic: show and read, the F32 casts (truncating, 0
# past the range or for NaN), the Nat casts (0n below 0, wrapping past
# 2^32), the i literal in an operator chain, and a match on i literals.
import Base

def lo() -> I32:
  I32.sub(I32.neg(2147483647i), 1i)

def rd.show(m: Maybe<&2, I32>) -> String:
  match m:
    case Some{v}:
      I32.show(v)
    case None{}:
      "None"

def rd(s: String) -> String:
  rd.show(I32.read(s))

def name(x: I32) -> String:
  match x:
    case 0i:
      "zero"
    case 1i:
      "one"
    case _:
      "other"

def ops(+a: I32, +b: I32) -> List<I32>:
  [(a * b - 10i / b : I32), (a - b >> 1n : I32), (a % b : I32),
    Bool.pick(I32, (a < b : I32), a, b)]

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(I32.show(0i))
    IO.print(I32.show(I32.neg(42i)))
    IO.print(I32.show(2147483647i))
    IO.print(I32.show(lo()))
    IO.print(rd("-17"))
    IO.print(rd("-2147483648"))
    IO.print(rd("2147483648"))
    IO.print(rd("-2147483649"))
    IO.print(rd("-"))
    IO.print(rd("12a"))
    IO.print(F32.show(I32.to_f32(I32.neg(5i))))
    IO.print(F32.show(I32.to_f32(lo())))
    IO.print(I32.show(F32.to_i32(F32.neg(2.5))))
    IO.print(I32.show(F32.to_i32(F32.neg(2147483648.0))))
    IO.print(I32.show(F32.to_i32(2147483648.0)))
    IO.print(I32.show(F32.to_i32(F32.div(0.0, 0.0))))
    IO.print(String.append(name(0i), name(I32.neg(1i))))
    IO.print(Nat.show(I32.to_nat(2147483647i)))
    IO.print(Nat.show(I32.to_nat(lo())))
    IO.print(I32.show(I32.from_nat(3000000000n)))
    IO.print(List.show(~&1, ~I32, ~I32.show, ops(I32.neg(3i), 4i)))

#|0
#|-42
#|2147483647
#|-2147483648
#|-17
#|-2147483648
#|None
#|None
#|None
#|None
#|-5
#|-2147483600
#|-2
#|-2147483648
#|0
#|0
#|zeroother
#|2147483647
#|0
#|-1294967296
#|[-14, -4, -3, -3]
