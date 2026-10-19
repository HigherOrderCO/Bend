# the same lambda under a bare domain: the instance is app's body at
# it, where the duplicated x is consumed twice, and the usage rule rejects
# it there, at the binder's span (a use points at its binder)
import Base

def app(~f: Nat -> Nat, x: Nat) -> Nat:
  f(x)

def main() -> Nat:
  app(~(y => Nat.add(y, y)), 3n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : x
#|- observed : x (consumed more than once)
#|Location: app~inst
#|5 |
#|6>| def app(~f: Nat -> Nat, x: Nat) -> Nat:
#|  |                         ^
#|7 |   f(x)
#|exit 1
