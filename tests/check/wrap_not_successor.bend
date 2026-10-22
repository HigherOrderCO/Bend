# the wrap wall, respelled: 1 + x in an EXPRESSION is U32.add, never a
# successor witness -- f(1 + 4294967295) wraps to f(0) = 7 at runtime,
# so the free-x equation must stay stuck and {==} must refuse. bend
# spells the arms as a literal-chain pattern and a catch-all; no Inc
# exists anywhere to close the add
import Base

law f:
  for n: U32
  U32

def f(n):
  match n:
    case 0:
      7
    case p:
      0

law bad:
  for x: U32
  {f((1 + x : U32)) == 0 : U32}

def bad(x):
  {==}

law main:
  U32

def main():
  0

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Context:
#|- x : U32
#|Location: bad
#|23 | def bad(x):
#|24>|   {==}
#|   |   ^^^^
#|25 |
#|- left     : f(U32.add(1, x))  =>  f(U32.add(1, x))
#|- right    : 0  =>  0
#|exit 1
