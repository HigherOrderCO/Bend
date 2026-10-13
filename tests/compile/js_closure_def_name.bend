# A def named like a JS local is no capture of the closure that calls it,
# though an earlier closure of the same def took that local's name.
import Base

law Foo._x_0:
  U32 -> U32

def Foo._x_0():
  a => U32.add(a, 1)

law apply:
  (U32 -> U32) -> U32 -> U32

def apply():
  f => x => f(x)

law go:
  for +n: Nat
  U32

def go(n):
  match n:
    case 0n:
      apply(z => U32.add(z, 2), 0)
    case 1n+p:
      apply(y => Foo._x_0(U32.add(y, U32.from_nat(p))), 1)

def main() -> IO(Unit):
  do IO<Unit>:
    t : Nat <- IO.now()
    IO.print(U32.show(go(Nat.add(5n, Nat.mod(t, 1n)))))

#|6
