# Native bit widths agree with Base at zero and bit-width boundaries.
import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(Nat.show(U32.log2(0)))
    IO.print(Nat.show(U32.log2(1)))
    IO.print(Nat.show(U32.log2(2)))
    IO.print(Nat.show(U32.log2(3)))
    IO.print(Nat.show(U32.log2(4)))
    IO.print(Nat.show(U32.log2(2147483647)))
    IO.print(Nat.show(U32.log2(2147483648)))
    IO.print(Nat.show(U32.log2(4294967295)))
    IO.print(Nat.show(Map.msb.u(0n, 4294967295)))
    IO.print(Nat.show(Map.msb.u(1n, 0)))
    IO.print(Nat.show(Map.msb.u(1n, 4294967295)))
    IO.print(Nat.show(Map.msb.u(31n, 4294967295)))
    IO.print(Nat.show(Map.msb.u(32n, 4294967295)))
    IO.print(Nat.show(Map.msb.u(99n, 4294967295)))
    IO.print(Nat.show(Map.msb.u(32n, 1)))
    IO.print(Nat.show(Map.msb.u(99n, 2147483648)))

#|0
#|0
#|1
#|1
#|2
#|30
#|31
#|31
#|0
#|0
#|1
#|31
#|32
#|32
#|1
#|32
