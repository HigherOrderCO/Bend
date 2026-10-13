# the number kit: Nat comparisons, min/max/div/mod/pow; U32 min/max/
# clamp/pow/is_even; F32 min/max/clamp/lerp/square/hypot/round/pi and
# the Nat conversions (floats read back scaled through F32.to_u32);
# Bool.pick, Bool.to_u32 and Bool.show
import Base

def fu(x: F32) -> String:
  U32.show(F32.to_u32(F32.mul(x, 100.0)))

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Bool.show(Nat.is_eq(3n, 3n)))
    IO.print(Bool.show(Nat.is_ne(3n, 3n)))
    IO.print(Bool.show(Nat.is_le(2n, 3n)))
    IO.print(Bool.show(Nat.is_gt(2n, 3n)))
    IO.print(Bool.show(Nat.is_ge(3n, 3n)))
    IO.print(Nat.show(Nat.min(4n, 9n)))
    IO.print(Nat.show(Nat.max(4n, 9n)))
    IO.print(Nat.show(Nat.div(17n, 5n)))
    IO.print(Nat.show(Nat.mod(17n, 5n)))
    IO.print(Nat.show(Nat.div(17n, 0n)))
    IO.print(Nat.show(Nat.pow(2n, 10n)))
    IO.print(U32.show(U32.min(4, 9)))
    IO.print(U32.show(U32.max(4, 9)))
    IO.print(U32.show(U32.clamp(15, 0, 10)))
    IO.print(U32.show(U32.clamp(5, 0, 10)))
    IO.print(U32.show(U32.pow(3, 4n)))
    IO.print(Bool.show(U32.is_even(6)))
    IO.print(Bool.show(U32.is_even(7)))
    IO.print(fu(F32.min(1.5, 2.5)))
    IO.print(fu(F32.max(1.5, 2.5)))
    IO.print(fu(F32.clamp(7.0, 0.0, 1.0)))
    IO.print(fu(F32.lerp(0.0, 10.0, 0.25)))
    IO.print(fu(F32.square(3.0)))
    IO.print(fu(F32.hypot(3.0, 4.0)))
    IO.print(fu(F32.round(2.5)))
    IO.print(fu(F32.round(2.4)))
    IO.print(fu(F32.round(0.4999999701976776)))
    IO.print(U32.show(F32.to_u32(F32.round(8388609.0))))
    IO.print(Bool.show(F32.is_eq(F32.round(F32.neg(8388609.0)), F32.neg(8388609.0))))
    IO.print(Bool.show(F32.is_eq(F32.round(F32.neg(2.5)), F32.neg(2.0))))
    IO.print(fu(F32.pi()))
    IO.print(Nat.show(F32.to_nat(F32.from_nat(42n))))
    IO.print(U32.show(Bool.pick(U32, True{}, 1, 2)))
    IO.print(U32.show(Bool.to_u32(True{})))
    IO.print(Bool.show(False{}))

#|True
#|False
#|True
#|False
#|True
#|4
#|9
#|3
#|2
#|0
#|1024
#|4
#|9
#|10
#|5
#|81
#|True
#|False
#|150
#|250
#|100
#|250
#|900
#|500
#|300
#|200
#|0
#|8388609
#|True
#|True
#|314
#|42
#|1
#|1
#|False
