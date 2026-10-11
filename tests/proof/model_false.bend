import Base
law L:
  for ~f: @A: Type -> A -> A
  for ~h: @B: Type -> @c: B -> @b: B -> {f(B, b) == c : B}
  for x: Nat
  {f(Nat, x) == 0n : Nat}
def L(f, h, x):
  h(Nat, 0n, x)

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
