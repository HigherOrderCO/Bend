# the same lambda under a bare domain: the instance app~0 is app's body at
# it, where the duplicated x is consumed twice, and the usage rule rejects
# it there, at the binder's span (a use points at its binder)
import Base

def app(~f: Nat -> Nat, x: Nat) -> Nat:
  f(x)

def main() -> Nat:
  app(~(y => Nat.add(y, y)), 3n)

#|SOME PROOFS FAIL
#|Error:
#|- message  : x appears 2 times (at 7:5, 7:5), but a plain binding is used at most once
#|Location: app~0
#|6 | def app(~f: Nat -> Nat, x: Nat) -> Nat:
#|7>|   f(x)
#|  |     ^
#|8 |
#|Note: a +x binding may be used many times if its type is Data; otherwise use it once (a function or another Type value cannot be copied).
#|exit 1
