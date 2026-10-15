# U32.pow keeps zero exponents and arithmetic modulo 2^32.
import Base

law pow_zero:
  for a: U32
  {U32.pow(a, 0n) == 1 : U32}

def pow_zero(a):
  {==}

law pow_succ:
  for +a: U32
  for +n: Nat
  {U32.pow(a, 1n+n) == U32.mul(a, U32.pow(a, n)) : U32}

def pow_succ(a, n):
  {==}

def main() -> List<U32>:
  [
    U32.pow(0, 0n),
    U32.pow(0, 1n),
    U32.pow(0, 32n),
    U32.pow(1, 0n),
    U32.pow(1, 33n),
    U32.pow(2, 0n),
    U32.pow(2, 1n),
    U32.pow(2, 2n),
    U32.pow(2, 3n),
    U32.pow(2, 31n),
    U32.pow(2, 32n),
    U32.pow(2, 33n),
    U32.pow(3, 4n),
    U32.pow(3, 5n),
    U32.pow(3, 16n),
    U32.pow(3, 34n),
    U32.pow(123456789, 17n),
    U32.pow(65536, 2n),
    U32.pow(2147483647, 2n),
    U32.pow(2147483648, 1n),
    U32.pow(2147483648, 2n),
    U32.pow(4294967294, 2n),
    U32.pow(4294967295, 1n),
    U32.pow(4294967295, 2n),
    U32.pow(4294967295, 3n)
  ]
#|[1, 0, 0, 1, 1, 1, 2, 4, 8, 2147483648, 0, 0, 81, 243, 43046721, 4077925001, 2192224469, 0, 1, 2147483648, 0, 4, 4294967295, 1, 4294967295]
