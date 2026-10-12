# Parallel vector lengths must stay finite and nonzero at extreme scales.
import Base

def lengths(+n: Nat, +s: F32) -> U32:
  match n:
    case 0n:
      +x = F32.hypot(F32.mul(3.0, s), F32.mul(4.0, s))
      Bool.to_u32(Bool.and(F32.is_gt(x, 0.0), F32.is_lt(x, F32.div(1.0, 0.0))))
    case 1n+p:
      a b = lengths(p, s) lengths(p, F32.neg(s))
      U32.add(a, b)

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(U32.show(lengths!(4n, 1.0e30)))
    IO.print(U32.show(lengths!(4n, 1.0e-30)))

#|16
#|16
