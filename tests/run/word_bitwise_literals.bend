# bitwise ops, strict &&/|| normalizing to 0/1, char-code words
# ported from bend3: hex literals become decimals (bend has no hex;
# 3735928559 is checked against 0xDEAD * 65536 + 0xBEEF), && and ||
# become land/lor over U32.is_ne, char words are checked through
# Char.to_u32 (nonzero is spelled not(is_eq(x, 0)): tocl lacks
# u32_is_ne, see run_word_compare's yellow)
import Base

law bu:
  for b: Bool
  U32

def bu(b):
  match b:
    case False{}:
      0
    case True{}:
      1

law land:
  for a: U32
  for b: U32
  U32

def land(a, b):
  bu(Bool.and(Bool.not(U32.is_eq(a, 0)), Bool.not(U32.is_eq(b, 0))))

law lor:
  for a: U32
  for b: U32
  U32

def lor(a, b):
  bu(Bool.or(Bool.not(U32.is_eq(a, 0)), Bool.not(U32.is_eq(b, 0))))

law main.out:
  U32

def main.out():
  b0 = bu(U32.is_eq(U32.and(61680, 65280), 61440))
  b1 = bu(U32.is_eq(U32.or(61680, 3855), 65535))
  b2 = bu(U32.is_eq(U32.xor(65280, 4080), 61680))
  b3 = bu(U32.is_eq(U32.add(U32.mul(57005, 65536), 48879), 3735928559))
  b4 = bu(U32.is_eq(Char.to_u32('A'), 65))
  b5 = bu(U32.is_eq(Char.to_u32('z'), 122))
  b6 = bu(U32.is_eq(Char.to_u32('0'), 48))
  b7 = bu(U32.is_eq(land(2, 3), 1))
  b8 = bu(U32.is_eq(land(0, 9), 0))
  b9 = bu(U32.is_eq(lor(7, 0), 1))
  b10 = bu(U32.is_eq(lor(0, 0), 0))
  U32.add(U32.add(U32.add(U32.add(U32.add(U32.add(U32.add(U32.add(U32.add(U32.add(b0, b1), b2), b3), b4), b5), b6), b7), b8), b9), b10)

law main:
  IO(Unit)

def main():
  IO.print(U32.show(main.out()))

#|11
