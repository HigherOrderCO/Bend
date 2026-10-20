import Base
law f:
  for n: Nat
  for x: Nat
  Nat
def g(n: Nat) -> Nat -> Nat:
  x =>
    -dummy = {y => f(n, x) : Nat -> Nat}
    x
def f(n, x):
  match n:
    case Zero{}:
      x
    case Succ{p}:
      g(p, x)
def main() -> Nat:
  f(1n, 7n)
def claim() -> {main() == 7n : Nat}:
  {==}

#|7n
