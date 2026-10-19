# A mention in a type position (an equation's endpoints) is dead: the
# measure does not count it, so the message must not either.

import Base

def f(x: Nat) -> Nat:
  y = {x == x : Nat}
  Nat.add(x, x)

#|SOME PROOFS FAIL
#|Error:
#|- message  : x is used 2 times (at 8:11, 8:14), but a plain binding is used at most once
#|Location: f
#|7 |   y = {x == x : Nat}
#|8>|   Nat.add(x, x)
#|  |              ^
#|9 |
#|Note: a +x binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
