# a compile-time argument naming a def declared later: the argument
# checks dead, but the instance calls the def live before its body is
# checked, and the refusal names the file order, as for any forward call
import Base

def app(~f: Nat -> Nat, x: Nat) -> Nat:
  f(x)

def main() -> Nat:
  app(~later, 3n)

def later(n: Nat) -> Nat:
  Nat.mul(n, 10n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : later
#|Context:
#|- x : Nat
#|Location: app~0
#| 9 | def main() -> Nat:
#|10>|   app(~later, 3n)
#|   |        ^^^^^
#|11 |
#|exit 1
