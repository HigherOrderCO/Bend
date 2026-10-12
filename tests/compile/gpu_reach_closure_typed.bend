# a bang over a pair of datatypes whose parameter is a datatype value
# that holds a type, the second naming U32 -> U32. The second Hold is a
# datatype the closure walk has seen, so it walks only its argument: a
# datatype's value may carry a type, so the walk opens TyB{U32 -> U32}
# and finds the function. So the lambda minted on the host is on the
# device, and the value is the plain call's
import Base

type TyB is Type:
  TyB{T: Type}

def TyB.get(b: TyB) -> Type:
  match b:
    case TyB{T}:
      T

type Hold<b: TyB> is Type:
  Hold{v: TyB.get(b)}

law app:
  for h: Hold<TyB{U32 -> U32}>
  U32

def app(h):
  Hold{v} = h
  v(40)

law run:
  for p: Hold<TyB{U32}> & Hold<TyB{U32 -> U32}>
  U32

def run(p):
  (a, h) = p
  Hold{x} = a
  (x + app(h) : U32)

law main.out:
  Nat

def main.out():
  U32.to_nat(run!((Hold{1}, Hold{y => U32.add(y, 2)})))

law main:
  IO(Unit)

def main():
  IO.print(String.append(Nat.show(main.out()), "n"))

#|43n
