# go's self-call passes g : Type -> Nat at its Kind(&0) -> Nat column:
# bend2's LE compares domains swapped and fits Kind(&0) to Type, but the
# kernel converts under a binder, and *(.Q0) is not *(.Q1). The domains
# differ by bend2's EQ, so the argument goes to --verdict eta-long, and
# the kernel fits the kinds at the leaf
import Base

def g(A: Type) -> Nat:
  0n

def go(n: Nat, f: Kind(&0) -> Nat) -> Nat:
  match n:
    case 0n: 0n
    case 1n+p: go(p, g)

def main() -> Nat:
  go(3n, g)

#|0n
