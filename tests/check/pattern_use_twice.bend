# A pattern field used twice: the + goes on the parameter the pattern
# matched, not on the field it names.

import Base

def twice(n: Nat) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+p:
      Nat.add(p, p)

#|SOME PROOFS FAIL
#|Error:
#|- message  : p is used 2 times (at 11:15, 11:18), but a plain binding is used at most once
#|Location: twice
#|10 |     case 1n+p:
#|11>|       Nat.add(p, p)
#|   |                  ^
#|12 |
#|Note: a +p binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|Note: p is bound by a pattern: put the + on the matched parameter, +n.
#|exit 1
