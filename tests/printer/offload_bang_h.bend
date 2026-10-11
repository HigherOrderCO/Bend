# f!(x) marks a reference for offloading (Ref.b, the fork-let's mark);
# the printer dropped the mark, so a shown def read as a plain call and
# the bang did not survive a print-parse round trip
import Base

# f matches, so f!(n) on a variable is stuck and the snf keeps the mark
def f(n: Nat) -> Nat:
  match n:
    case 0n:
      0n
    case 1n+p:
      p

def g(n: Nat) -> Nat:
  f!(n)

def main(+n: Nat) -> Nat:
  a b = f!(n) f(n)
  Nat.add(a, b)

#|n => Nat.add(f!(n), f(n))
