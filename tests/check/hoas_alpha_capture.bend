# soundness pin: HOAS conversion never confuses binders -- the K vs
# K* combinator equation is refused (a capture bug would accept it).
# ported from bend3
import Base

law bad:
  {(x => y => x) == (x => y => y) : U32 -> U32 -> U32}

def bad(): {==}

law main:
  U32

def main(): 0

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Location: bad
#| 8 |
#| 9>| def bad(): {==}
#|   |            ^^^^
#|10 |
#|- left     : x => y => x  =>  x => y => x
#|- right    : x => y => y  =>  x => y => y
#|exit 1
