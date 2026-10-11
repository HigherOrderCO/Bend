# a meet is live code, and the kernel counts a variable's uses in both
# of its sides: a is used once in each arm of f's match, once inside a
# meet, so --verdict counts two uses and a rides into the arms
import Base

def keep(q: Quant, -A: Kind(q), x: A) -> A:
  x

def f(a: Quant, b: Bool) -> Nat:
  match b:
    case True{}: keep(a <&> &1, Nat, 1n)
    case False{}: keep(a, Nat, 2n)

def main() -> Nat:
  f(&2, True{})

#|1n
