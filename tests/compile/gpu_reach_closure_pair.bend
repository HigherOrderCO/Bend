# a bang over a pair whose second half is a pair holding a closure: a
# pair's second half is a type family, which the closure walk opens, so
# the lambda minted on the host is on the device, and the value is the
# plain call's
import Base

law app:
  for q: U32 & (U32 -> U32)
  U32

def app(q):
  (x, f) = q
  f(x)

law run:
  for p: U32 & U32 & (U32 -> U32)
  U32

def run(p):
  (x, q) = p
  (x + app(q) : U32)

law main.out:
  Nat

def main.out():
  U32.to_nat(run!((1, (40, y => U32.add(y, 2)))))

law main:
  IO(Unit)

def main():
  IO.print(String.append(Nat.show(main.out()), "n"))

#|43n
