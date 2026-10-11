# g's and h's ~ arguments differ only in a fallback past Unit's last
# constructor, dead code that never runs; --verdict skips the arm as bend2
# does, and agrees (#1204)
import Base

def app(~f: Unit -> Nat) -> Nat:
  f(Unit{})

def g(x: Nat) -> Nat:
  app(~{\{Unit: 0n; x} : Unit -> Nat})

def h(x: Bool) -> Nat:
  app(~{\{Unit: 0n; x} : Unit -> Nat})

def main() -> Nat:
  Nat.add(g(42n), h(True{}))

#|0n
