# t's rewrite has type F(0n) = Nat -> Kind(&0), at a Nat -> Type goal:
# bend2's LE fits them under the binder, but the kernel fits a rewrite's
# type to its goal by conversion there. So the rewrite goes to --verdict
# at its own type, and the kernel fits the kinds at the eta-long leaf
import Base

def K0(A: Nat) -> Kind(&0):
  Nat

def F(n: Nat) -> Type:
  Nat -> Kind(&0)

def t(n: Nat, e: {n == 0n : Nat}) -> Nat -> Type:
  %e : F(_)
  K0

def main() -> Nat:
  0n

#|0n
