import Base
law f:
  for n: Nat
  Nat -> Nat
def g(n: Nat, x: Nat) -> Nat:
  -dummy = {y => f(n, x) : Nat -> Nat}
  x
def f(n):
  match n:
    case Zero{}:
      x => x
    case Succ{p}:
      x => g(p, x)
def main() -> Nat:
  f(1n, 7n)
def claim() -> {main() == 7n : Nat}:
  {==}

#|7n
