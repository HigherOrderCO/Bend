# A template retains its complete argument, including the fallback past
# Unit's last constructor. That fallback must be closed before the
# argument can enter the global instance cache, even though it is dead.
import Base

def app(~f: Unit -> Nat) -> Nat:
  f(Unit{})

def g(x: Nat) -> Nat:
  app(~{\{Unit: 0n; x} : Unit -> Nat})

def h(x: Bool) -> Nat:
  app(~{\{Unit: 0n; x} : Unit -> Nat})

def main() -> Nat:
  Nat.add(g(42n), h(True{}))

#|SOME PROOFS FAIL
#|Error:
#|- expected : a template applied to closed ~ arguments (including retained match fallbacks)
#|- observed : app
#|Context:
#|- x : Nat
#|Location: g
#| 9 | def g(x: Nat) -> Nat:
#|10>|   app(~{\{Unit: 0n; x} : Unit -> Nat})
#|   |   ^^^
#|11 |
#|exit 1
