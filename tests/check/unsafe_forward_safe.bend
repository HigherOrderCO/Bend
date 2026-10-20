# an @unsafe def calls a def declared after it; a safe def after it may
# not, and the refusal names the file order, not a law (#1782)
import Base

@unsafe
def f(x: Nat) -> Nat:
  g(x)

def h(x: Nat) -> Nat:
  g(x)

def g(x: Nat) -> Nat:
  x

def main() -> Nat:
  h(1n)

#|SOME PROOFS FAIL
#|Error:
#|- expected : a def written above its use (defs are checked in file order)
#|- observed : g
#|Context:
#|- x : Nat
#|Location: h
#| 9 | def h(x: Nat) -> Nat:
#|10>|   g(x)
#|   |   ^
#|11 |
#|exit 1
