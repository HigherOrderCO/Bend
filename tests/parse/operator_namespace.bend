# ( .. : T) names every operator of the operator expression it annotates:
# the whole chain, through grouping parens and && / ||; an index's U32 and
# an array literal's T name their own chains the same way
import Base
import ../import/alias_lib.bend as M

def at(r: Array<U32> & U32) -> U32:
  (xs, x) = r
  x

# Empty calls retain the source name for match labels and method namespaces.
def flag() -> Bool -> U32:
  \{True(): 1; False(): 0}

def main() -> U32:
  xs = [1 + 2 : U32*4n]
  a  = (1 + (2 * 3) - 1 : U32)
  b  = flag(((1 < 2) && (2 < 3) : U32))
  (a + b + at(xs[1 + 1]) + U32.from_nat((1n + 2n : M())) : U32)

#|10
