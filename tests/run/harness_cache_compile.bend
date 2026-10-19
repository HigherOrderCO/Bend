# gates/test.ts's state_run compiles a second checked copy of this book
# in one process, C and JS over and over, around normalization and the
# next fixture: each must give the first copy's generated code.
import Base

law double:
  for n: Nat
  Nat

def double(n):
  match n:
    case Zero{}:
      0n
    case Succ{k}:
      Succ{Succ{double(k)}}

law main:
  IO(Unit)

def main():
  IO.print(Nat.show(double(3n)))

#|6
