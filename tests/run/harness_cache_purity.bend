# gates/test.ts's state_run compiles a second checked copy of this book
# in one process, C and JS over and over, around normalization and the
# next fixture: each must give the first copy's bytes.
import Base

law grow:
  for n: Nat
  Nat

def grow(n):
  match n:
    case Zero{}:
      1n
    case Succ{k}:
      Succ{Succ{grow(k)}}

law shrink:
  for n: Nat
  Nat

def shrink(n):
  match n:
    case Zero{}:
      0n
    case Succ{k}:
      k

law main:
  IO(Unit)

def main():
  IO.print(Nat.show(shrink(grow(5n))))

#|10
