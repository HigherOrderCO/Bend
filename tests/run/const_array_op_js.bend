# A constant array passed to an op the JS lane writes as a template that
# names its argument twice is held in a local, so Array.set writes the
# array the op returns, not a copy.
import Base

def pick(p: Array<U32> & U32) -> U32:
  (a, x) = p
  x

def main() -> U32:
  pick(Array.get(U32, Array.set(U32, ALeaf{5}, 0, 7), 0))

#|7
