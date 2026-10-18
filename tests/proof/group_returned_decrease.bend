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
  match x:
    case Zero{}:
      n
    case Succ{p}:
      g(n, p)
def main() -> Nat:
  f(7n, 1n)
def claim() -> {main() == 0n : Nat}:
  {==}

#|0n
