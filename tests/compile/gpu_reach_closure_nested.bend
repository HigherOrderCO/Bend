# a bang over a nested datatype: each link doubles the type its next link
# holds. The walk opens a datatype once per value of its value arguments
# and walks its type arguments apart, so it ends at once, not after
# 2^1000 pairs
import Base

type P<A: Type, B: Type> is Type:
  P{a: A, b: B}

type Grow<A: Type> is Type:
  Stop{x: A}
  More{next: Grow<P<A, A>>}

def sum(g: Grow<U32>) -> U32:
  match g:
    case Stop{x}:
      x
    case More{n}:
      0

def main() -> IO(Unit):
  IO.print(U32.show(sum!(Stop{7})))

#|7
