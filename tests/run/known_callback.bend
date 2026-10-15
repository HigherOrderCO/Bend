# Literal callbacks reuse a scalar capture and open a tuple pattern.
# The C compiler can fuse these calls; all lanes must still return 42.
import Base

def apply(x: U32, callback: U32 -> U32) -> U32:
  callback(x)

def with_pair(result: U32 & U32, callback: U32 -> U32 -> U32) -> U32:
  (a, b) = result
  callback(a, b)

def run(+bias: U32, x: U32) -> U32:
  a = apply(x, y => U32.add(y, bias))
  b = with_pair((a, 2), y => z => U32.add(y, z))
  c = apply(b, y => U32.add(y, bias))
  apply(c, y => U32.add(y, bias))

def main() -> U32:
  run(3, 31)

#|42
