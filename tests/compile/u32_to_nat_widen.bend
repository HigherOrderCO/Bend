# U32.to_nat of an unboxed u32 local widens before Nat arithmetic: with the
# u32 kept as a C local (used twice), 1n+U32.to_nat(x) and a Nat.add of two
# converted words must not wrap at 2^32
import Base

def succ(+x: U32) -> String:
  U32.show(x) ++ " " ++ Nat.show(1n+U32.to_nat(x))

def sum(+x: U32, +y: U32) -> String:
  U32.show(x) ++ " " ++ U32.show(y) ++ " " ++ Nat.show(Nat.add(U32.to_nat(x), U32.to_nat(y)))

def main() -> IO(Unit):
  IO.print(succ(4294967295) ++ "; " ++ sum(4294967295, 4294967295))

#|4294967295 4294967296; 4294967295 4294967295 8589934590
