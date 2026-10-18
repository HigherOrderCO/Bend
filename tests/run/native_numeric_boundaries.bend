# Native scalar paths agree with Base at zero, bit-width and Nat boundaries.
import Base

def main() -> IO(Unit):
  +largest = Nat.add(Nat.mul(16777215n, 16777216n), 16777215n)
  do IO<Unit>:
    IO.print(Nat.show(Nat.pow(0n, 0n)))
    IO.print(Nat.show(Nat.pow(0n, 7n)))
    IO.print(Nat.show(Nat.pow(3n, 7n)))
    IO.print(Nat.show(Nat.pow(1n, 257n)))
    IO.print(Nat.show(Nat.pow(2n, 47n)))
    IO.print(Nat.show(Nat.pow(largest, 0n)))
    IO.print(Nat.show(Nat.pow(largest, 1n)))
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

#|1
#|0
#|2187
#|1
#|140737488355328
#|1
#|281474976710655
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