# I32's signed natives inside a `!`: 2^16 lanes, each on its own hashed word
# (negatives, and a zero divisor every 256 lanes), summed with wrapping. The
# device runs the same C natives as the host, so it prints what the CPU and
# JS lanes print.
import Base

def ord(c: Cmp) -> I32:
  match c:
    case LT{}:
      I32.neg(1i)
    case EQ{}:
      0i
    case GT{}:
      1i

def leaf(+i: U32) -> I32:
  +x = I32.from_u32(U32.mul(i, 2654435761))
  +y = I32.sub(I32.from_u32(U32.and(i, 255)), 128i)
  +q = I32.xor(I32.div(x, y), I32.mod(x, y))
  +s = I32.shrn(x, U32.to_nat(U32.and(i, 31)))
  +f = F32.to_i32(I32.to_f32(I32.shrn(x, 8n)))
  I32.add(I32.add(I32.add(q, s), I32.add(f, ord(I32.cmp(x, y)))),
    I32.add(I32.min(x, y), I32.abs(y)))

def sum(+d: Nat, +i: U32) -> I32:
  match d:
    case 0n:
      leaf(i)
    case 1n+p:
      a b = sum(p, U32.shl(i)) sum(p, U32.inc(U32.shl(i)))
      I32.add(a, b)

def main() -> IO(Unit):
  IO.print(I32.show(sum!(16n, 0)))

#|1894206013
