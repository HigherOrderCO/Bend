# a bang over a nested datatype whose index grows with each link: every
# link is a new instance, and its type argument doubles. The walk meets
# the shared halves of P<A, A> once, so it reaches the cap 1000 deep
# in time linear in the depth, not after 2^depth pairs
import Base

type P<A: Type, B: Type> is Type:
  P{a: A, b: B}

type Gr<A: Type, -n: Nat> is Type:
  Stop{x: A}
  More{next: Gr<P<A, A>, (1n + n : Nat)>}

def sum(g: Gr<U32, 0n>) -> U32:
  match g:
    case Stop{x}:
      x
    case More{n}:
      0

def main() -> IO(Unit):
  IO.print(U32.show(sum!(Stop{7})))

#|7
