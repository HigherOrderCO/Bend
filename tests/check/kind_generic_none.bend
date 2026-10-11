# a generic kind at &0 and a literal Kind(&0) go to --verdict as one term,
# *(.Q0): the kernel builds K(&0)'s kind from K's *(q), so a literal &0 is
# not raised to *(.Q1). The law holds in bend2 (EQ) and in the kernel
# (conv), and go's self-call passes K(&0) at a Nat -> Type column
# eta-long, as bend2 fits Kind(&0) to Type only by LE
import Base

def K(-q: Quant, A: Nat) -> Kind(q):
  Nat

def Kd(-q: Quant) -> Type:
  Kind(q)

law kd_none:
  {Kd(&0) == Kind(&0) : Type}

def kd_none():
  {==}

def go(n: Nat, f: Nat -> Type) -> Nat:
  match n:
    case 0n: 0n
    case 1n+p: go(p, K(&0))

def main() -> Nat:
  go(3n, K(&0))

#|0n
