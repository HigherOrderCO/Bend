# a bang over a pair whose second half is a dependent pair, whose own
# second half is a value-indexed datatype holding a closure. The inner
# pair is a datatype the closure walk has seen, so it walks only its
# arguments: it skips the quantities and the index, which are values, but
# still opens the family that names Fn. So the lambda minted on the host
# is on the device, and the value is the plain call's
import Base

type Fn<-n: Nat> is Type:
  Fn{f: U32 -> U32}

law app:
  for q: Exists(Nat, n => Fn<n>)
  U32

def app(q):
  (n, b) = q
  Fn{f} = b
  f(40)

law run:
  for p: U32 & Exists(Nat, n => Fn<n>)
  U32

def run(p):
  (x, q) = p
  (x + app(q) : U32)

law main.out:
  Nat

def main.out():
  U32.to_nat(run!((1, (3n, Fn{y => U32.add(y, 2)}))))

law main:
  IO(Unit)

def main():
  IO.print(String.append(Nat.show(main.out()), "n"))

#|43n
