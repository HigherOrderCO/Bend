# a compile-time argument naming a def declared later: the argument
# checks dead, but the instance calls the def live, and a def below is
# a bodiless claim there, as for any forward call
import Base

def app(~f: Nat -> Nat, x: Nat) -> Nat:
  f(x)

def main() -> Nat:
  app(~later, 3n)

def later(n: Nat) -> Nat:
  Nat.mul(n, 10n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a filled definition (an unfilled law is a dead claim: live code cannot use it)
#|- observed : later
#|Context:
#|- x : Nat
#|Location: app~inst
#| 9 | def main() -> Nat:
#|10>|   app(~later, 3n)
#|   |        ^^^^^
#|11 |
#|exit 1
