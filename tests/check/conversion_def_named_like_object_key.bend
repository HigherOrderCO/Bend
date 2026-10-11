# a def may be named like a key of a plain JS object: the pass that
# compares both sides with no def unfolded must not find toString there
import Base

def toString(n: Nat) -> Nat:
  Nat.add(n, 1n)

def t(+n: Nat) -> {toString(n) == Nat.add(n, 1n) : Nat}:
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
