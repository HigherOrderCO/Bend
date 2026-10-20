# Compare direct calls, partial applications, and large exponents.
import Base

def apply(f: U32 -> U32, a: U32) -> U32:
  f(a)

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(U32.show(apply(a => U32.pow(a, 0n), 0)))
    IO.print(U32.show(apply(a => U32.pow(a, 34n), 3)))
    IO.print(U32.show(U32.pow(2, 4096n)))
    IO.print(U32.show(U32.pow(3, 4096n)))
    IO.print(U32.show(U32.pow(4294967295, 4095n)))
    IO.print(U32.show(U32.pow(123456789, 4095n)))
#|1
#|4077925001
#|0
#|1989099521
#|4294967295
#|1431689789
