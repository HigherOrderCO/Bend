# Byte and Word16 over Word: arithmetic wraps at 8 and 16 bits, U32 views
# take the low bits, and a U32 splits into bytes and back
import Base

def b(n: Nat) -> Byte:
  Byte.from_nat(n)

def round_trip() -> {U32.of_bytes(U32.byte(305419896, 0n), U32.byte(305419896, 1n), U32.byte(305419896, 2n), U32.byte(305419896, 3n)) == 305419896 : U32}:
  {==}

def be() -> {U32.of_bytes_be(b(18n), b(52n), b(86n), b(120n)) == 305419896 : U32}:
  {==}

def wraps() -> {Byte.add(b(250n), b(10n)) == b(4n) : Byte}:
  {==}

def main() -> List<U32>:
  [
    Byte.to_u32(Byte.add(b(250n), b(10n))),
    Byte.to_u32(Byte.sub(b(3n), b(5n))),
    Byte.to_u32(Byte.mul(b(16n), b(17n))),
    Byte.to_u32(Byte.xor(b(170n), b(255n))),
    Byte.to_u32(Byte.shl(b(129n))),
    Byte.to_u32(U32.byte(305419896, 0n)),
    Byte.to_u32(U32.byte(305419896, 3n)),
    Word16.to_u32(Word16.add(U32.to_word16(65535), U32.to_word16(2))),
    Word16.to_u32(Word16.of_bytes(b(52n), b(18n))),
    U32.of_bytes(b(120n), b(86n), b(52n), b(18n)),
    U32.of_bytes_be(b(18n), b(52n), b(86n), b(120n))
  ]

#|[4, 254, 16, 85, 2, 120, 18, 1, 4660, 305419896, 305419896]
