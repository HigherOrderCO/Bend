# A match on Nat misses Zero: the note spells the case as surface syntax
# (0n), not as the internal constructor.

import Base

def pred(n: Nat) -> Nat:
  match n:
    case 1n+p:
      p

#|SOME PROOFS FAIL
#|Error:
#|- message  : this match doesn't cover Zero
#|Location: pred
#|6 | def pred(n: Nat) -> Nat:
#|7>|   match n:
#|  |   ^^^^^^^^
#|8 |     case 1n+p:
#|Note: add `case 0n:` (or `case _:`).
#|exit 1
