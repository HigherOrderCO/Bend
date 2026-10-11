import Base
law L:
  for ~f: @A: Type -> A -> A
  for ~h: @B: Type -> @c: Nat -> @b: B -> {f(B, b) == b : B}
  for x: Nat
  {f(Nat, x) == x : Nat}
def L(f, h, x):
  h(Nat, 0n, x)

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
