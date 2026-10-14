# go's self-call passes Kq(q) : Nat -> Kind(q) at its Nat -> Type column:
# bend2 fits them, but the kernel converts kinds under a binder, and *(q)
# is not *(.Q1). The two types' kinds differ as the kernel reads them, so
# the argument goes to --verdict eta-long, and the kernel fits Kind(q) to
# Type at the leaf
import Base

def Kq(-q: Quant, A: Nat) -> Kind(q):
  Nat

def go(-q: Quant, n: Nat, f: Nat -> Type) -> Nat:
  match n:
    case 0n: 0n
    case 1n+p: go(q, p, Kq(q))

def main() -> Nat:
  go(&2, 3n, Kq(&2))

#|0n
