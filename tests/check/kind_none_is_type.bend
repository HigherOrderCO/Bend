# bend2's Kind(&0) fits Type (LE) but is not Type (EQ), and --verdict emits
# it as the kernel's *(.Q0), which fits *(.Q1) and does not convert to it.
# go's self-call passes K0 : Nat -> Kind(&0) at its Nat -> Type column, so
# the argument goes eta-long, and the kernel fits the kinds at the leaf
import Base

def K0(A: Nat) -> Kind(&0):
  Nat

def go(n: Nat, f: Nat -> Type) -> Nat:
  match n:
    case 0n: 0n
    case 1n+p: go(p, K0)

def main() -> Nat:
  go(3n, K0)

#|0n
